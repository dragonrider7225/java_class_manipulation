//! A library to parse a Java class file.

#![feature(associated_type_defaults)]
#![feature(box_patterns, box_syntax)]

use extended_io as eio;

use nom::Err;

use num_enum::{TryFromPrimitive, TryFromPrimitiveError};

use std::{
    convert::TryInto,
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    io::{self, Read, Write},
};

#[macro_use]
mod parsers;

/// Fragments of a Java class file.
pub mod fragment;

/// Java types.
pub mod types;

use fragment::{
    constant_pool::{CPAccessError, ConstantPool},
    ClassFileVersion, JavaAttribute, JavaField, JavaIdentifier, JavaMethod, RawAttribute, RawField,
    RawMethod,
};

use parsers::{jvm8, NomFlatError};

use types::{JavaType, QualifiedClassName};

/// The return type of all functions in this crate that return a `Result`.
pub type CrateResult<T> = Result<T, CrateError>;

/// The error type of all functions in this crate that return a `Result`.
#[derive(Debug)]
pub enum CrateError {
    /// The wrapper around I/O errors.
    IoError(io::Error),
    /// The wrapper around class parse errors.
    ParseError(ClassParseError),
    /// A wrapper.
    TryFromIntError(std::num::TryFromIntError),
    /// A wrapper.
    CPAccessError(CPAccessError),
    /// A wrapper.
    NomError(NomFlatError),
    /// A wrapper.
    Str(&'static str),
    /// A wrapper.
    String(String),
}

impl Display for CrateError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::IoError(e) => Display::fmt(e, f),
            Self::ParseError(e) => Display::fmt(e, f),
            Self::TryFromIntError(e) => Display::fmt(e, f),
            Self::CPAccessError(e) => Display::fmt(e, f),
            Self::NomError(e) => Display::fmt(e, f),
            Self::Str(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "{}", s),
        }
    }
}

impl Error for CrateError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::IoError(e) => Some(e),
            Self::ParseError(e) => Some(e),
            Self::TryFromIntError(e) => Some(e),
            Self::CPAccessError(e) => Some(e),
            Self::NomError(e) => Some(e),
            Self::Str(_) | Self::String(_) => None,
        }
    }
}

impl From<io::Error> for CrateError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

impl From<ClassParseError> for CrateError {
    fn from(cpe: ClassParseError) -> Self {
        match cpe {
            ClassParseError::IoError(e) => e.into(),
            ClassParseError::NomError(e) => e.into(),
            ClassParseError::CPAccessError(e) => e.into(),
            e => Self::ParseError(e),
        }
    }
}

impl From<std::num::TryFromIntError> for CrateError {
    fn from(e: std::num::TryFromIntError) -> Self {
        Self::TryFromIntError(e)
    }
}

impl From<CPAccessError> for CrateError {
    fn from(e: CPAccessError) -> Self {
        Self::CPAccessError(e)
    }
}

impl From<NomFlatError> for CrateError {
    fn from(e: NomFlatError) -> Self {
        Self::NomError(e)
    }
}

impl From<&'static str> for CrateError {
    fn from(e: &'static str) -> Self {
        Self::Str(e)
    }
}

impl From<String> for CrateError {
    fn from(e: String) -> Self {
        Self::String(e)
    }
}

impl<E> From<TryFromPrimitiveError<E>> for CrateError
where
    E: Debug,
    E: TryFromPrimitive,
{
    fn from(e: TryFromPrimitiveError<E>) -> Self {
        Self::String(format!("{:?}", e))
    }
}

/// Convert an error into an IO error.
pub fn mk_io_error<E>(e: E) -> io::Error
where
    E: Into<Box<dyn Error + Send + Sync>>,
{
    io::Error::new(io::ErrorKind::Other, e)
}

/// A generic two-type discriminated union.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Either<L, R> {
    /// The container for values of type `L`.
    Left(L),
    /// The container for values of type `R`.
    Right(R),
}

impl<L, R> Either<L, R> {
    /// Map the left value by `f` if `self` is `Left`. Otherwise do nothing.
    pub fn map_left<F, O>(self, f: F) -> Either<O, R>
    where
        F: FnOnce(L) -> O,
    {
        match self {
            Self::Left(l) => Either::Left(f(l)),
            Self::Right(r) => Either::Right(r),
        }
    }

    /// Map the right value by `f` if `self` is `Right`. Otherwise do nothing.
    pub fn map_right<F, O>(self, f: F) -> Either<L, O>
    where
        F: FnOnce(R) -> O,
    {
        match self {
            Self::Left(l) => Either::Left(l),
            Self::Right(r) => Either::Right(f(r)),
        }
    }
}

impl<T> Either<T, T> {
    /// Return the inner value.
    pub fn unwrap(self) -> T {
        match self {
            Self::Left(ret) | Self::Right(ret) => ret,
        }
    }
}

/// An error which arises in the process of parsing a Java class file.
#[derive(Debug)]
pub enum ClassParseError {
    /// The expected magic number at the beginning of the class file is not present
    InvalidMagicNumber {
        /// The actual bytes in the Java class file where the magic number 0xCAFE_BABE should be.
        actual: u32,
    },
    /// The constant pool entry tag is invalid.
    InvalidConstantPoolEntryTag {
        /// The actual byte read where there should be a constant pool entry tag.
        actual: u8,
    },
    /// A byte sequence was invalid.
    InvalidByteSequence {
        /// A description of the set of valid byte sequences in this location.
        expected: String,
        /// The actual byte sequence.
        actual: Vec<u8>,
    },
    /// An attribute with a specified length that is invalid for that attribute.
    InvalidAttributeLength {
        /// The name of the attribute with invalid specified length.
        name: String,
        /// The invalid specified length.
        len: u32,
    },
    /// An attribute with a specified value that is invalid for that attribute.
    InvalidAttributeValue {
        /// The name of the attribute with invalid specified value.
        name: String,
        /// The invalid specified value.
        value: String,
    },
    /// A wrapper
    GenericError(Box<dyn Error + 'static + Send + Sync>),
    /// A wrapper
    IoError(io::Error),
    /// A wrapper
    CPAccessError(CPAccessError),
    /// A wrapper
    NomError(NomFlatError),
}

impl From<io::Error> for ClassParseError {
    fn from(e: io::Error) -> Self {
        Self::IoError(e)
    }
}

impl From<CPAccessError> for ClassParseError {
    fn from(e: CPAccessError) -> Self {
        Self::CPAccessError(e)
    }
}

impl From<NomFlatError> for ClassParseError {
    fn from(base: NomFlatError) -> ClassParseError {
        ClassParseError::NomError(base)
    }
}

macro_rules! impl_from_err_for_from_nom_error {
    ($($t:ty)*) => ($(
        // This should ideally be covered by some blanket impl, such as
        // ```
        // impl<T, U, V> From<T> for V
        // where
        //     U: From<T>,
        //     V: From<U>,
        // {
        //     fn from(base: T) -> V {
        //         V::from(U::from(base))
        //     }
        // }
        // ```
        // but I suspect that that would result in too much complexity for the
        // type checker to handle.
        impl<E: Debug> From<Err<E>> for $t {
            fn from(base: Err<E>) -> $t {
                <$t as From<NomFlatError>>::from(NomFlatError::from(base))
            }
        }
    )*)
}

impl_from_err_for_from_nom_error!(ClassParseError);

impl Display for ClassParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ClassParseError::InvalidMagicNumber { actual } => {
                const EXPECTED: u32 = 0xCAFE_BABE;
                write!(
                    f,
                    "Invalid magic number: Expected {}, found {}",
                    EXPECTED, actual,
                )
            }
            ClassParseError::InvalidConstantPoolEntryTag { actual } => write!(
                f,
                "Invalid constant pool entry tag: {}, found {}",
                "Expected constant pool entry tag", actual,
            ),
            ClassParseError::InvalidByteSequence { expected, actual } => write!(
                f,
                r#"Invalid byte sequence: {} "{}", found "{:?}""#,
                "Expected byte sequence to match", expected, actual,
            ),
            ClassParseError::InvalidAttributeLength { name, len } => {
                write!(f, "Invalid length ({}) for attribute {}", len, name)
            }
            ClassParseError::InvalidAttributeValue { name, value } => {
                write!(f, r#"Invalid value ("{}") for attribute {}"#, value, name,)
            }
            ClassParseError::GenericError(e) => write!(f, "Error: {}", e),
            ClassParseError::IoError(e) => write!(f, "I/O Error: {}", e),
            ClassParseError::CPAccessError(e) => write!(f, "Error accessing constant pool: {}", e),
            ClassParseError::NomError(e) => write!(f, "Error in nom: {}", e),
        }
    }
}

impl Error for ClassParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ClassParseError::InvalidMagicNumber { .. }
            | ClassParseError::InvalidConstantPoolEntryTag { .. }
            | ClassParseError::InvalidByteSequence { .. }
            | ClassParseError::InvalidAttributeLength { .. }
            | ClassParseError::InvalidAttributeValue { .. } => None,
            ClassParseError::GenericError(e) => Some(e.as_ref()),
            ClassParseError::IoError(e) => Some(e),
            ClassParseError::CPAccessError(e) => Some(e),
            ClassParseError::NomError(e) => Some(e),
        }
    }
}

fn convert_jvm8_to_string(bytes: &[u8]) -> Result<String, ClassParseError> {
    Ok(jvm8::parse_jvm8(bytes)?.1)
}

fn write_jvm8(sink: &mut dyn Write, s: &str) -> io::Result<()> {
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        match c.into() {
            0u32 => eio::write_u16(sink, 0xC080)?,
            0x01..=0x7F => eio::write_u8(sink, c as u8)?,
            0x80..=0x7FF => {
                // At least 8 bits but less than 12.
                // 0x110ccccc 0x10cccccc
                let mut buf = [0; 2];
                c.encode_utf8(&mut buf);
                eio::write_byte_slice(sink, &buf[..])?
            }
            0x800..=0xFFFF => {
                // At least 12 bits but less than 17.
                // 0x1110cccc 0x10cccccc 0x10cccccc
                let mut buf = [0; 3];
                c.encode_utf8(&mut buf);
                eio::write_byte_slice(sink, &buf[..])?
            }
            0x1_0000..=0x10_FFFF => {
                // At least 17 bits but less than 22.
                // Regular UTF-8 encoding is 0x11110ccc 0x10cccccc 0x10cccccc 0x10cccccc
                let mut buf = [0u8; 6];
                let bits: u32 = c.into();
                buf[5] = 0x80 | (bits & 0x3F) as u8;
                let bits = bits >> 6;
                buf[4] = 0xB0 | (bits & 0xF) as u8;
                let bits = bits >> 4;
                buf[3] = 0xED;
                buf[2] = 0x80 | (bits & 0x3F) as u8;
                let bits = bits >> 6;
                buf[1] = 0xA0 | (bits - 1) as u8;
                buf[0] = 0xED;
                eio::write_byte_slice(sink, &buf[..])?
            }
            _ => break,
        }
    }
    Ok(())
}

/// A symbolic reference to a field in some Java class.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FieldRef {
    owner: QualifiedClassName,
    ident: JavaIdentifier,
    r#type: JavaType,
}

impl FieldRef {
    fn new(owner: QualifiedClassName, ident: JavaIdentifier, r#type: JavaType) -> Self {
        FieldRef {
            owner,
            ident,
            r#type,
        }
    }

    fn owner(&self) -> QualifiedClassName {
        self.owner.clone()
    }

    fn ident(&self) -> JavaIdentifier {
        self.ident.clone()
    }

    fn r#type(&self) -> JavaType {
        self.r#type.clone()
    }
}

/// A symbolic reference to a method in some Java class.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MethodRef {
    interface_method: bool,
    owner: QualifiedClassName,
    ident: JavaIdentifier,
    r#type: JavaType,
}

impl MethodRef {
    fn new(
        interface_method: bool,
        owner: QualifiedClassName,
        ident: JavaIdentifier,
        r#type: JavaType,
    ) -> Self {
        Self {
            interface_method,
            owner,
            ident,
            r#type,
        }
    }

    fn is_interface_method(&self) -> bool {
        self.interface_method
    }

    fn owner(&self) -> QualifiedClassName {
        self.owner.clone()
    }

    fn ident(&self) -> JavaIdentifier {
        self.ident.clone()
    }

    fn r#type(&self) -> JavaType {
        self.r#type.clone()
    }
}

/// A type which has access flags.
pub trait AccessFlagged {
    /// The class, field, or method is declared public: it may be accessed anywhere the declaring
    /// class or class file can be accessed.
    fn is_public(&self) -> bool; // 0x0001
    /// The class, field, or method is declared private: it may not be accessed from outside of the
    /// class where it is declared; it must not be a top-level class.
    fn is_private(&self) -> bool; // 0x0002
    /// The class, field, or method is declared protected: it may not be accessed by any class which
    /// is neither in the same package nor a subclass of the declaring class; it must not be a top-
    /// level class.
    fn is_protected(&self) -> bool; // 0x0004
    /// The class, field, or method does not have an explicit visibility: it may not be accessed
    /// outside of the package.
    fn is_package_protected(&self) -> bool {
        !self.is_public() && !self.is_private() && !self.is_protected()
    }
    /// The class, field, or method is declared static. A static class must not be top-level.
    fn is_static(&self) -> bool; // 0x0008
    /// The class, field, or method is declared final.
    fn is_final(&self) -> bool; // 0x0010
    /// The JVM should ignore the class name in all uses of invokespecial in this class file.
    /// Beginning with 7u13, Oracle's JVM treated all class files as if this attribute were present,
    /// regardless of whether it actually is.
    fn is_super_special(&self) -> bool; // 0x0020
    /// The method is declared synchronized. A class or field cannot be synchronized.
    fn is_synchronized(&self) -> bool; // 0x0020
    /// The field is declared volatile: it must not be cached. A class or method cannot be volatile.
    fn is_volatile(&self) -> bool; // 0x0040
    /// The method is a bridge method. A bridge method is generated when a generic method is
    /// "overridden" by a method with a different erasure. For example, given the following classes,
    /// the compiler erases Node<T>.setData(T) to Node.setData(Object) and generates a method
    /// MyNode.setData(Object) which calls MyNode.setData(Integer) with the proper casts.
    ///
    /// ```java
    /// public class Node<T> { // The compiler erases this into Node
    ///     private T data; // The compiler erases this into Object data
    ///
    ///     public Node(T data) { // The compiler erases this into Node(Object)
    ///         this.data = data;
    ///     }
    ///
    ///     public void setData(T data) { // The compiler erases this into setData(Object)
    ///         this.data = data;
    ///     }
    /// }
    ///
    /// public class MyNode extends Node<Integer> {
    ///     public MyNode(Integer data) {
    ///         super(data + 1);
    ///     }
    ///
    ///     @Override
    ///     public void setData(Integer data) {
    ///         super.setData(data + 1);
    ///     }
    /// }```
    ///
    /// `MyNode.setData(Object)` is generated as a delegate to `MyNode.setData(Integer)` so that
    /// calling setData on a MyNode object with a non-Integer argument will produce an exception
    /// instead of behaving badly in silence.
    ///
    /// A class or field cannot be a bridge.
    fn is_bridge(&self) -> bool; // 0x0040
    /// A transient field is not serialized by the default Java serialization functions. A transient
    /// field should be re-initialized on deserialization by either the no-args constructor (if it
    /// is not dependent on the values of non-transient fields) or a custom deserializer with
    /// signature and exceptions `private void readObject(ObjectInputStream) throws IOException,
    /// ClassNotFoundException`.
    fn is_transient(&self) -> bool; // 0x0080
    /// The method has variable arity. A class or field cannot be varargs.
    fn is_varargs(&self) -> bool; // 0x0080
    /// The method was not implemented in Java. A class or field cannot be native.
    fn is_native(&self) -> bool; // 0x0100
    /// The class is an interface. A method or field cannot be an interface.
    fn is_interface(&self) -> bool; // 0x0200
    /// The class or method is abstract. An abstract class cannot be instantiated directly. An
    /// abstract method does not have a body and must be implemented by any instantiable subclass of
    /// its parent class. A field cannot be abstract.
    fn is_abstract(&self) -> bool; // 0x0400
    /// Require all floating point operations in the method or class to remain adherent to IEEE 754
    /// in all intermediate steps. Default Java behavior is to allow extended exponent range.
    fn is_strict(&self) -> bool; // 0x0800
    /// The class, method, or field is not present in the associated source.
    fn is_synthetic(&self) -> bool; // 0x1000
    /// The class is an annotation type: declared as "@interface".
    fn is_annotation(&self) -> bool; // 0x2000
    /// The class or field is an enum or enum constant.
    fn is_enum(&self) -> bool; // 0x4000
}

/// A Java class object.
pub struct JavaClass {
    version: ClassFileVersion,
    access_flags: u16,
    this_class: QualifiedClassName,
    super_class: QualifiedClassName,
    interfaces: Vec<QualifiedClassName>,
    fields: Vec<JavaField>,
    methods: Vec<JavaMethod>,
    attributes: Vec<JavaAttribute>,
}

fn read_interfaces(
    src: &mut dyn Read,
    constant_pool: &ConstantPool,
) -> CrateResult<Vec<QualifiedClassName>> {
    let num_interfaces = eio::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_interfaces as usize);
    for _ in 0..num_interfaces {
        let class_idx = eio::read_u16(src)?;
        ret.push(constant_pool.get_class_name(class_idx)?);
    }
    Ok(ret)
}

impl JavaClass {
    /// Create a Java class object from information about the class.
    pub fn new(
        version: ClassFileVersion,
        access_flags: u16,
        this_class: QualifiedClassName,
        super_class: QualifiedClassName,
        interfaces: Vec<QualifiedClassName>,
        fields: Vec<JavaField>,
        methods: Vec<JavaMethod>,
        attributes: Vec<JavaAttribute>,
    ) -> JavaClass {
        JavaClass {
            version,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        }
    }

    /// Read a Java class object from a Java class file.
    pub fn read(src: &mut dyn Read) -> CrateResult<JavaClass> {
        // Class file structure:
        // {
        //     // Must be 0xCAFE_BABE
        //     magic_number: u32,
        //     minor_version: u16,
        //     major_version: u16,
        //     // 1+ Number of elements in the constant pool
        //     constant_pool_length: u16,
        //     // Indexed from 1
        //     constant_pool: ConstantPool,
        //     access_flags: u16,
        //     // Index of the CPEntry::Class(..) instance for this class
        //     this_class: u16,
        //     super_class: u16,
        //     num_interfaces: u16,
        //     interfaces: [u16; num_interfaces],
        //     num_fields: u16,
        //     fields: [JavaField; num_fields],
        //     num_methods: u16,
        //     methods: [RawMethod; num_methods],
        //     num_attributes: u16,
        //     attributes: [Attribute; num_attributes],
        // }
        match eio::read_u32(src)? {
            0xCAFE_BABE => {}
            n => return Ok(Err(ClassParseError::InvalidMagicNumber { actual: n })?),
        }
        let version = {
            let minor_version = eio::read_u16(src)?;
            let major_version = eio::read_u16(src)?;
            ClassFileVersion::new(major_version, minor_version)
        };
        let constant_pool = ConstantPool::read(src)?;
        let access_flags = eio::read_u16(src)?;
        let this_class = {
            let this_class_idx = eio::read_u16(src)?;
            constant_pool.get_class_name(this_class_idx)?
        };
        let super_class = {
            let super_class_idx = eio::read_u16(src)?;
            constant_pool.get_class_name(super_class_idx)?
        };
        let interfaces = read_interfaces(src, &constant_pool)?;
        let fields = fragment::read_fields(src, &constant_pool)?;
        let methods = fragment::read_methods(src, &constant_pool)?;
        let attributes = fragment::read_attributes(src, &constant_pool)?;
        let ret = JavaClass::new(
            version,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        );
        Ok(ret)
    }

    /// Write the Java class object to a Java class file.
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        // Class file structure:
        // {
        //     // Must be 0xCAFE_BABE
        //     magic_number: u32,
        //     minor_version: u16,
        //     major_version: u16,
        //     // 1+ Number of elements in the constant pool
        //     constant_pool_length: u16,
        //     // Indexed from 1
        //     constant_pool: ConstantPool,
        //     access_flags: u16,
        //     // Index of the CPEntry::Class(..) instance for this class
        //     this_class: u16,
        //     super_class: u16,
        //     num_interfaces: u16,
        //     interfaces: [u16; num_interfaces],
        //     num_fields: u16,
        //     fields: [JavaField; num_fields],
        //     num_methods: u16,
        //     methods: [RawMethod; num_methods],
        //     num_attributes: u16,
        //     attributes: [Attribute; num_attributes],
        // }
        let mut pool = ConstantPool::default();
        let this_class_idx = pool.add_class_name(self.this_class)?;
        let super_class_idx = pool.add_class_name(self.super_class)?;
        let interface_idxs = pool.add_interfaces(self.interfaces)?;
        let raw_fields: Vec<RawField> = self
            .fields
            .into_iter()
            .map(|field| field.into_raw(&mut pool))
            .collect::<Result<_, _>>()?;
        let raw_methods: Vec<RawMethod> = self
            .methods
            .into_iter()
            .map(|method| method.into_raw(&mut pool))
            .collect::<Result<_, _>>()?;
        let raw_attributes: Vec<RawAttribute> = self
            .attributes
            .into_iter()
            .map(|attribute| attribute.into_raw(&mut pool))
            .collect::<Result<_, _>>()?;

        eio::write_u32(sink, 0xCAFE_BABE)?;
        eio::write_u16(sink, self.version.minor_version())?;
        eio::write_u16(sink, self.version.major_version())?;
        pool.write(sink)?;
        eio::write_u16(sink, self.access_flags)?;
        eio::write_u16(sink, this_class_idx)?;
        eio::write_u16(sink, super_class_idx)?;
        eio::write_u16(sink, interface_idxs.len().try_into()?)?;
        for interface_idx in interface_idxs {
            eio::write_u16(sink, interface_idx)?;
        }
        eio::write_u16(sink, raw_fields.len().try_into()?)?;
        for raw_field in raw_fields {
            raw_field.write(sink)?;
        }
        eio::write_u16(sink, raw_methods.len().try_into()?)?;
        for raw_method in raw_methods {
            raw_method.write(sink)?;
        }
        eio::write_u16(sink, raw_attributes.len().try_into()?)?;
        for raw_attribute in raw_attributes {
            raw_attribute.write(sink)?;
        }
        Ok(())
    }

    /// Get the version number of the class.
    pub fn get_class_file_version(&self) -> ClassFileVersion {
        self.version
    }

    /// Get the name of the class.
    pub fn get_name(&self) -> &QualifiedClassName {
        &self.this_class
    }

    /// Get the name of the non-interface class directly extended by the class.
    pub fn get_superclass_name(&self) -> &QualifiedClassName {
        &self.super_class
    }

    /// Get the names of the interfaces directly implemented by the class.
    pub fn get_interface_names(&self) -> &[QualifiedClassName] {
        &self.interfaces[..]
    }

    /// Get the fields of the class.
    pub fn fields(&self) -> &[JavaField] {
        &self.fields[..]
    }

    /// Get the methods of the class.
    pub fn methods(&self) -> &[JavaMethod] {
        &self.methods[..]
    }

    /// Get the attributes of the class.
    pub fn attributes(&self) -> &[JavaAttribute] {
        &self.attributes[..]
    }
}

impl AccessFlagged for JavaClass {
    fn is_public(&self) -> bool {
        self.access_flags & 0x0001 != 0
    }

    fn is_private(&self) -> bool {
        self.access_flags & 0x0002 != 0
    }

    fn is_protected(&self) -> bool {
        self.access_flags & 0x0004 != 0
    }

    fn is_static(&self) -> bool {
        self.access_flags & 0x0008 != 0
    }

    fn is_final(&self) -> bool {
        self.access_flags & 0x0010 != 0
    }

    fn is_super_special(&self) -> bool {
        self.access_flags & 0x0020 != 0
    }

    fn is_synchronized(&self) -> bool {
        false
    }

    fn is_volatile(&self) -> bool {
        false
    }

    fn is_bridge(&self) -> bool {
        false
    }

    fn is_transient(&self) -> bool {
        false
    }

    fn is_varargs(&self) -> bool {
        false
    }

    fn is_native(&self) -> bool {
        false
    }

    fn is_interface(&self) -> bool {
        self.access_flags & 0x0200 != 0
    }

    fn is_abstract(&self) -> bool {
        self.access_flags & 0x0400 != 0
    }

    fn is_strict(&self) -> bool {
        false
    }

    fn is_synthetic(&self) -> bool {
        self.access_flags & 0x1000 != 0
    }

    fn is_annotation(&self) -> bool {
        self.access_flags & 0x2000 != 0
    }

    fn is_enum(&self) -> bool {
        self.access_flags & 0x4000 != 0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn java_type_parser_fails_on_empty() {
        assert!("".parse::<JavaType>().is_err());
    }

    #[test]
    fn java_type_parses_byte() {
        let src = "B";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Byte));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_char() {
        let src = "C";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Char));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_double() {
        let src = "D";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Double));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_float() {
        let src = "F";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Float));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_int() {
        let src = "I";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Int));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_long() {
        let src = "J";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Long));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn java_type_parses_class_name_default_package() {
        let src = "LClass;";
        let expected = Ok(JavaType::Class(QualifiedClassName::ClassFile(
            PackageName::DEFAULT_PACKAGE,
            JavaIdentifier::new("Class").unwrap(),
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn parses_class_name_Object() {
        let src = "Ljava/lang/Object;";
        let expected = Ok(JavaType::Class(QualifiedClassName::ClassFile(
            PackageName(vec![
                JavaIdentifier::new("java").unwrap(),
                JavaIdentifier::new("lang").unwrap(),
            ]),
            JavaIdentifier::new("Object").unwrap(),
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_short() {
        let src = "S";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Short));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_bool() {
        let src = "Z";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::Bool));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_array_int() {
        let src = "[I";
        let expected = JavaType::mk_array(JavaType::Primitive(JavaPrimitive::Int))
            .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_array_array_bool() {
        let src = "[[Z";
        let bool_array = JavaType::mk_array(JavaType::Primitive(JavaPrimitive::Bool)).unwrap();
        let expected = JavaType::mk_array(bool_array).ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_func_thunk() {
        let src = "()V";
        let expected = JavaType::mk_method(vec![], JavaType::Primitive(JavaPrimitive::Void))
            .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_func_int_void() {
        let src = "(I)V";
        let arg_types = vec![JavaType::Primitive(JavaPrimitive::Int)];
        let expected = JavaType::mk_method(arg_types, JavaType::Primitive(JavaPrimitive::Void))
            .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[allow(non_snake_case)]
    #[test]
    fn parses_func_array_int_int_Object() {
        let src = "([II)Ljava/lang/Object;";
        let arg_types = vec![
            JavaType::Class(QualifiedClassName::Array(Either::Left(JavaPrimitive::Int))),
            JavaType::Primitive(JavaPrimitive::Int),
        ];
        let ret_type = JavaType::Class(QualifiedClassName::ClassFile(
            PackageName(vec![
                JavaIdentifier::new("java").unwrap(),
                JavaIdentifier::new("lang").unwrap(),
            ]),
            JavaIdentifier::new("Object").unwrap(),
        ));
        let expected = JavaType::mk_method(arg_types, ret_type).ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_aaload() -> Result<(), io::Error> {
        let src = [JavaOpCode::AALOAD];
        let expected = JavaOpCode::Aaload;
        let result =
            JavaOpCode::nom_parse(&Default::default(), &src[..]).map_err(NomFlatError::from)?;
        assert_eq!(expected, result.1?);
        Ok(())
    }
}
