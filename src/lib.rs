#![feature(box_syntax, box_patterns)]

use nom::{
    self,
    branch,
    bytes::complete as bytes,
    combinator as comb,
    multi,
    sequence,
    Err,
    IResult,
};

use std::{
    borrow::Borrow,
    convert::{TryFrom, TryInto},
    error::Error,
    fmt::{
        self,
        Debug,
        Display,
        Formatter,
    },
    io::{
        self,
        Read,
        Write,
    },
    str::FromStr,
};

#[macro_use]
mod parsers;

use parsers::{jvm8, NomParse};

fn mk_io_error<E>(e: E) -> io::Error
where
    E: Error + 'static + Sized + Send + Sync,
{
    io::Error::new(io::ErrorKind::Other, e)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

impl<E, R> From<Result<R, E>> for Either<E, R> {
    fn from(base: Result<R, E>) -> Self {
        match base {
            Ok(res) => Either::Right(res),
            Err(e) => Either::Left(e),
        }
    }
}

#[derive(Debug)]
struct NomError {
    e: String
}

impl Display for NomError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.e)
    }
}

impl<E: Debug> From<Err<E>> for NomError {
    fn from(e: Err<E>) -> NomError {
        NomError {
            e: format!("{:?}", e),
        }
    }
}

impl Error for NomError {}

#[derive(Debug)]
pub enum ClassParseError {
    /// arg is the four bytes from the class file that are expected to be the
    /// magic number but aren't.
    InvalidMagicNumber {actual: u32},
    /// arg is the byte that is expected to be the tag for an entry in the
    /// constant pool but is not a valid tag.
    InvalidConstantPoolEntryTag {actual: u8},
    /// first arg is a description of the expected byte sequence(s), second arg
    /// is the actual byte sequence.
    InvalidByteSequence {expected: String, actual: Vec<u8>},
    /// A wrapper
    GenericError(Box<dyn Error + 'static>),
}

macro_rules! impl_from_err_for_from_nom_error {
    ($($t:ty)*) => ($(
        // This should really be covered by some blanket impl, such as
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
        impl<E: Debug> From<Err<E>> for $t {
            fn from(base: Err<E>) -> $t {
                <$t as From<NomError>>::from(NomError::from(base))
            }
        }
    )*)
}

impl_from_err_for_from_nom_error!(ClassParseError CPAccessError);

macro_rules! impl_from_error_for_class_parse_error {
    ($($t:ty)*) => ($(
        impl From<$t> for ClassParseError {
            fn from(base: $t) -> ClassParseError {
                ClassParseError::GenericError(box base)
            }
        }
    )*)
}

impl_from_error_for_class_parse_error!(NomError io::Error CPAccessError);

impl Display for ClassParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ClassParseError::InvalidMagicNumber {actual} => {
                const EXPECTED: u32 = 0xCAFE_BABE;
                write!(
                    f,
                    "Invalid magic number: Expected {}, found {}",
                    EXPECTED,
                    actual,
                )
            }
            ClassParseError::InvalidConstantPoolEntryTag {actual} => {
                write!(
                    f,
                    "Invalid constant pool entry tag: Expected constant pool entry tag, found {}",
                    actual,
                )
            }
            ClassParseError::InvalidByteSequence {expected, actual} => {
                write!(
                    f,
                    r#"Invalid byte sequence: Expected byte sequence to match "{}", found "{:?}""#,
                    expected,
                    actual
                )
            }
            ClassParseError::GenericError(e) => {
                write!(f, "Error: {}", e)
            }
        }
    }
}

impl Error for ClassParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            ClassParseError::InvalidMagicNumber { .. }
                    | ClassParseError::InvalidConstantPoolEntryTag { .. }
                    | ClassParseError::InvalidByteSequence { .. } => None,
            ClassParseError::GenericError(e) => Some(e.as_ref()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReferenceKind {
    ReadInstanceField,
    ReadStaticField,
    WriteInstanceField,
    WriteStaticField,
    InvokeVirtual,
    InvokeStatic,
    InvokeSpecial,
    InvokeNew,
    InvokeInterface,
}

impl TryFrom<u8> for ReferenceKind {
    type Error = ClassParseError;

    fn try_from(kind: u8) -> Result<ReferenceKind, ClassParseError> {
        match kind {
            1 => Ok(ReferenceKind::ReadInstanceField),
            2 => Ok(ReferenceKind::ReadStaticField),
            3 => Ok(ReferenceKind::WriteInstanceField),
            4 => Ok(ReferenceKind::WriteStaticField),
            5 => Ok(ReferenceKind::InvokeVirtual),
            6 => Ok(ReferenceKind::InvokeStatic),
            7 => Ok(ReferenceKind::InvokeSpecial),
            8 => Ok(ReferenceKind::InvokeNew),
            9 => Ok(ReferenceKind::InvokeInterface),
            n => Err(
                ClassParseError::InvalidByteSequence {
                    expected: r#"[\u0001-\u0009]"#.to_string(),
                    actual: vec![n],
                }
            ),
        }
    }
}

impl Into<u8> for ReferenceKind {
    fn into(self) -> u8 {
        match self {
            ReferenceKind::ReadInstanceField => 1,
            ReferenceKind::ReadStaticField => 2,
            ReferenceKind::WriteInstanceField => 3,
            ReferenceKind::WriteStaticField => 4,
            ReferenceKind::InvokeVirtual => 5,
            ReferenceKind::InvokeStatic => 6,
            ReferenceKind::InvokeSpecial => 7,
            ReferenceKind::InvokeNew => 8,
            ReferenceKind::InvokeInterface => 9,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JavaPrimitive {
    Void,
    Bool,
    Char,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
}

impl JavaPrimitive {
    fn to_internal_form(&self) -> String {
        match self {
            JavaPrimitive::Void => String::from("V"),
            JavaPrimitive::Bool => String::from("Z"),
            JavaPrimitive::Char => String::from("C"),
            JavaPrimitive::Byte => String::from("B"),
            JavaPrimitive::Short => String::from("S"),
            JavaPrimitive::Int => String::from("I"),
            JavaPrimitive::Long => String::from("J"),
            JavaPrimitive::Float => String::from("F"),
            JavaPrimitive::Double => String::from("D"),
        }
    }

    fn to_source_form(&self) -> String {
        match self {
            JavaPrimitive::Void => String::from("void"),
            JavaPrimitive::Bool => String::from("boolean"),
            JavaPrimitive::Char => String::from("char"),
            JavaPrimitive::Byte => String::from("byte"),
            JavaPrimitive::Short => String::from("short"),
            JavaPrimitive::Int => String::from("int"),
            JavaPrimitive::Long => String::from("long"),
            JavaPrimitive::Float => String::from("float"),
            JavaPrimitive::Double => String::from("double"),
        }
    }

    fn nom_available<'a>(available_types: AvailableTypes)
            -> impl Fn(&'a str) -> IResult<&'a str, JavaPrimitive> {

        move |s| {
            if available_types.is_void_available() {
                JavaPrimitive::nom_parse(s)
            } else {
                comb::map_opt(
                    JavaPrimitive::nom_parse,
                    |x| {
                        match x {
                            JavaPrimitive::Void => None,
                            x => Some(x),
                        }
                    },
                )(s)
            }
        }
    }
}

impl NomParse for JavaPrimitive {
    fn nom_parse(s: &str) -> IResult<&str, JavaPrimitive> {
        branch::alt((
            comb::value(JavaPrimitive::Void, bytes::tag("V")),
            comb::value(JavaPrimitive::Bool, bytes::tag("Z")),
            comb::value(JavaPrimitive::Char, bytes::tag("C")),
            comb::value(JavaPrimitive::Byte, bytes::tag("B")),
            comb::value(JavaPrimitive::Short, bytes::tag("S")),
            comb::value(JavaPrimitive::Int, bytes::tag("I")),
            comb::value(JavaPrimitive::Long, bytes::tag("J")),
            comb::value(JavaPrimitive::Float, bytes::tag("F")),
            comb::value(JavaPrimitive::Double, bytes::tag("D")),
        ))(s)
    }
}

#[derive(Clone, Copy)]
enum AvailableTypes {
    All,
    NoFunc,
    NoVoid,
    NoFuncNoVoid,
}

impl AvailableTypes {
    pub fn except_func(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc =>
                AvailableTypes::NoFunc,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFuncNoVoid,
        }
    }

    pub fn and_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => AvailableTypes::All,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFunc,
        }
    }

    pub fn except_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid =>
                AvailableTypes::NoVoid,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFuncNoVoid,
        }
    }

    pub fn is_void_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc => true,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid => false,
        }
    }

    pub fn is_func_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => true,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
    Primitive(JavaPrimitive),
    Class(QualifiedClassName),
    Method(Box<Vec<JavaType>>, Box<JavaType>),
}

impl JavaType {
    /// Returns None if el_type is JavaType::Method(..), otherwise converts the
    /// type into a QualifiedClassName for array of el_type and wraps it in the
    /// JavaType::Class constructor.
    pub fn mk_array(el_type: JavaType) -> Option<JavaType> {
        let ret = match el_type {
            JavaType::Primitive(el_type) => {
                Some(QualifiedClassName::primitive_array(el_type))
            }
            JavaType::Class(el_type) => {
                Some(QualifiedClassName::class_array(el_type))
            }
            JavaType::Method(..) => None,
        };
        ret.map(|x| JavaType::Class(x))
    }

    pub fn mk_method(arg_types: Vec<JavaType>, ret_type: JavaType) -> JavaType {
        JavaType::Method(box arg_types, box ret_type)
    }

    fn nom_method<'a>(available_types: AvailableTypes)
            -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {
    
        parsers::sane_cond(
            available_types.is_func_available(),
            comb::map(
                sequence::pair(
                    sequence::delimited(
                        bytes::tag("("),
                        multi::many0(
                            JavaType::nom_available(
                                available_types.except_func().except_void()
                            )
                        ), // Vec<JavaType>
                        bytes::tag(")")
                    ), // arg type list
                    JavaType::nom_available(
                        available_types.except_func().and_void()
                    ),
                ),
                |(arg_types, ret_type)| {
                    JavaType::mk_method(arg_types, ret_type)
                },
            ),
        )
    }

    fn nom_available(available_types: AvailableTypes)
            -> impl Fn(&str) -> IResult<&str, JavaType> {

        // Explicit anonymous function required to prevent "recursive function
        // type" error
        move |s| branch::alt((
            comb::map(
                JavaPrimitive::nom_available(available_types),
                |x| JavaType::Primitive(x),
            ),
            comb::map(QualifiedClassName::nom_parse, |x| JavaType::Class(x)),
            JavaType::nom_method(available_types),
        ))(s)
    }

    fn to_internal_form(&self) -> String {
        match self {
            JavaType::Primitive(primitive) => primitive.to_internal_form(),
            JavaType::Class(qcn) => format!("L{};", qcn.to_internal_form()),
            JavaType::Method(box arg_types, box ret_type) => {
                let arg_str: String = arg_types.iter().map(JavaType::to_internal_form).collect();
                format!("({}){}", arg_str, ret_type.to_internal_form())
            }
        }
    }
}

impl NomParse for JavaType {
    fn nom_parse(s: &str) -> IResult<&str, Self> {
        JavaType::nom_available(AvailableTypes::All.except_void())(s)
    }
}

impl_from_str_for_nom_parse!(JavaType);

#[derive(Debug)]
pub enum CPEntry {
    /// arg is modified UTF-8 string according to the following rules:
    /// Positive code points less than 0x80 are represented by 0b0ccccccc where
    /// the cs represent the least significant bits of the source code point.
    /// Other code points less than 0x800 are represented by 0b110ccccc
    /// 0b10cccccc where the cs are as previously.
    /// Other code points less than 0x1_0000 are represented by 0b1110cccc
    /// 0b10cccccc 0b10cccccc where the cs are as previously.
    /// Other code points are converted to their UTF-16 encodings and
    /// represented by the concatenation of the encodings of the surrogates.
    /// This has the effect of producing the bytes 0b11101101 0b1010dddd
    /// 0b10cccccc 0b11101101 0b1011cccc 0b10cccccc where the cs are as
    /// previously and the ds are what remains when the code point is shifted
    /// right by 16 bits then reduced by 1.
    Utf8(String),
    /// arg is the value.
    Integer(u32),
    /// arg is the value.
    Float(f32),
    /// arg is the value.
    Long(u64),
    /// arg is the value.
    Double(f64),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains
    /// the fully-qualified name of a class. If the class is an array class,
    /// such as int[] or java.lang.Thread[], the encoding is [I or
    /// [Ljava/lang/thread;. For any other fully qualified class name
    /// foo.bar.Baz, the encoding is Lfoo/bar/Baz;.
    Class(u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the contents of the string.
    String(u16),
    /// first arg is 1-based index of the Class entry for the class that owns
    /// the referenced field, second arg is 1-based index of the NameAndType
    /// entry of the referenced field.
    Fieldref(u16, u16),
    /// Same as Fieldref except that first arg must not be index of an
    /// interface and second arg must be a method.
    Methodref(u16, u16),
    /// Same as Methodref except that first arg must be index of an interface.
    InterfaceMethodref(u16, u16),
    /// first arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the name of the reference, second arg is 1-based index of a
    /// Utf8 entry in the constant pool which contains the type descriptor of
    /// the reference.
    NameAndType(u16, u16),
    /// first arg is reference kind, second arg is 1-based index of the
    /// {Field,Method,InterfaceMethod}ref in the constant pool to interact
    /// with.
    MethodHandle(ReferenceKind, u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the method descriptor.
    MethodType(u16),
    /// first arg is 0-based index into bootstrap methods array in the
    /// attributes table, second arg is 1-based index of a NameAndType entry
    /// representing the referenced method.
    InvokeDynamic(u16, u16),
    /// An empty constant pool entry representing the hole in the constant pool
    /// following the 8-byte constants Long and Double.
    After8Byte,
}

impl CPEntry {
    fn r#type(&self) -> CPEntryType {
        match self {
            CPEntry::Utf8(_) => CPEntryType::Utf8,
            CPEntry::Integer(_) => CPEntryType::Integer,
            CPEntry::Float(_) => CPEntryType::Float,
            CPEntry::Long(_) => CPEntryType::Long,
            CPEntry::Double(_) => CPEntryType::Double,
            CPEntry::Class(_) => CPEntryType::Class,
            CPEntry::String(_) => CPEntryType::String,
            CPEntry::Fieldref(_, _) => CPEntryType::Fieldref,
            CPEntry::Methodref(_, _) => CPEntryType::Methodref,
            CPEntry::InterfaceMethodref(_, _) => CPEntryType::InterfaceMethodref,
            CPEntry::NameAndType(_, _) => CPEntryType::NameAndType,
            CPEntry::MethodHandle(_, _) => CPEntryType::MethodHandle,
            CPEntry::MethodType(_) => CPEntryType::MethodType,
            CPEntry::InvokeDynamic(_, _) => CPEntryType::InvokeDynamic,
            CPEntry::After8Byte => CPEntryType::After8Byte,
        }
    }

    fn tag(&self) -> u8 {
        match self {
            CPEntry::Utf8(_) => 0x01,
            CPEntry::Integer(_) => 0x03,
            CPEntry::Float(_) => 0x04,
            CPEntry::Long(_) => 0x05,
            CPEntry::Double(_) => 0x06,
            CPEntry::Class(_) => 0x07,
            CPEntry::String(_) => 0x08,
            CPEntry::Fieldref(_, _) => 0x09,
            CPEntry::Methodref(_, _) => 0x0A,
            CPEntry::InterfaceMethodref(_, _) => 0x0B,
            CPEntry::NameAndType(_, _) => 0x0C,
            CPEntry::MethodHandle(_, _) => 0x0F,
            CPEntry::MethodType(_) => 0x10,
            CPEntry::InvokeDynamic(_, _) => 0x12,
            CPEntry::After8Byte => panic!("No tag for constant pool hole"),
        }
    }

    fn read(src: &mut dyn Read) -> Result<CPEntry, ClassParseError> {
        match extended_io::read_u8(src)? {
            0x01 => Ok(read_utf8_cp_entry(src)?),
            0x03 => Ok(CPEntry::Integer(extended_io::read_u32(src)?)),
            0x04 => Ok(CPEntry::Float(extended_io::read_f32(src)?)),
            0x05 => Ok(CPEntry::Long(extended_io::read_u64(src)?)),
            0x06 => Ok(CPEntry::Double(extended_io::read_f64(src)?)),
            0x07 => Ok(CPEntry::Class(extended_io::read_u16(src)?)),
            0x08 => Ok(CPEntry::String(extended_io::read_u16(src)?)),
            0x09 => {
                let owner_idx = extended_io::read_u16(src)?;
                let name_and_type_idx = extended_io::read_u16(src)?;
                Ok(CPEntry::Fieldref(owner_idx, name_and_type_idx))
            },
            0x0A => {
                let owner_idx = extended_io::read_u16(src)?;
                let name_and_type_idx = extended_io::read_u16(src)?;
                Ok(CPEntry::Methodref(owner_idx, name_and_type_idx))
            },
            0x0B => {
                let owner_idx = extended_io::read_u16(src)?;
                let name_and_type_idx = extended_io::read_u16(src)?;
                Ok(CPEntry::InterfaceMethodref(owner_idx, name_and_type_idx))
            },
            0x0C => {
                let name_idx = extended_io::read_u16(src)?;
                let type_idx = extended_io::read_u16(src)?;
                Ok(CPEntry::NameAndType(name_idx, type_idx))
            },
            0x0F => {
                let rk = ReferenceKind::try_from(extended_io::read_u8(src)?)?;
                let ref_idx = extended_io::read_u16(src)?;
                Ok(CPEntry::MethodHandle(rk, ref_idx))
            },
            0x10 => Ok(CPEntry::MethodType(extended_io::read_u16(src)?)),
            0x12 => {
                let bootstrap_method_idx = extended_io::read_u16(src)?;
                let name_and_type_idx = extended_io::read_u16(src)?;
                let ret = CPEntry::InvokeDynamic(
                    bootstrap_method_idx,
                    name_and_type_idx
                );
                Ok(ret)
            },
            n => Err(ClassParseError::InvalidConstantPoolEntryTag{actual: n}),
        }
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        extended_io::write_u8(sink, self.tag())?;
        match &self {
            &CPEntry::Utf8(s) => write_jvm8(sink, s)?,
            &CPEntry::Integer(n) => extended_io::write_u32(sink, *n)?,
            &CPEntry::Float(f) => extended_io::write_f32(sink, *f)?,
            &CPEntry::Long(n) => extended_io::write_u64(sink, *n)?,
            &CPEntry::Double(f) => extended_io::write_f64(sink, *f)?,
            &CPEntry::Class(idx) => extended_io::write_u16(sink, *idx)?,
            &CPEntry::String(idx) => extended_io::write_u16(sink, *idx)?,
            &CPEntry::Fieldref(idx1, idx2) | &CPEntry::Methodref(idx1, idx2)
                    | &CPEntry::InterfaceMethodref(idx1, idx2)
                    | &CPEntry::NameAndType(idx1, idx2)
                    | &CPEntry::InvokeDynamic(idx1, idx2) => {

                extended_io::write_u16(sink, *idx1)?;
                extended_io::write_u16(sink, *idx2)?;
            },
            &CPEntry::MethodHandle(rk, idx) => {
                extended_io::write_u8(sink, (*rk).into())?;
                extended_io::write_u16(sink, *idx)?;
            },
            &CPEntry::MethodType(idx) => extended_io::write_u16(sink, *idx)?,
            &CPEntry::After8Byte => {},
        }
        Ok(())
    }
}

impl PartialEq for CPEntry {
    fn eq(&self, other: &CPEntry) -> bool {
        match (self, other) {
            (CPEntry::Utf8(ref s), CPEntry::Utf8(ref t)) => s == t,
            (CPEntry::Integer(x), CPEntry::Integer(y)) => x == y,
            (CPEntry::Float(x), CPEntry::Float(y)) => x.to_bits() == y.to_bits(),
            (CPEntry::Long(x), CPEntry::Long(y)) => x == y,
            (CPEntry::Double(x), CPEntry::Double(y)) => x.to_bits() == y.to_bits(),
            (CPEntry::Class(idx1), CPEntry::Class(idx2)) => idx1 == idx2,
            (CPEntry::String(idx1), CPEntry::String(idx2)) => idx1 == idx2,
            (CPEntry::Fieldref(owner1, nat1), CPEntry::Fieldref(owner2, nat2))
                    | (CPEntry::Methodref(owner1, nat1), CPEntry::Methodref(owner2, nat2))
                    | (CPEntry::InterfaceMethodref(owner1, nat1), CPEntry::InterfaceMethodref(owner2, nat2)) => {

                owner1 == owner2 && nat1 == nat2
            }
            (CPEntry::NameAndType(name1, type1), CPEntry::NameAndType(name2, type2)) => name1 == name2 && type1 == type2,
            (CPEntry::MethodHandle(kind1, idx1), CPEntry::MethodHandle(kind2, idx2)) => kind1 == kind2 && idx1 == idx2,
            (CPEntry::MethodType(idx1), CPEntry::MethodType(idx2)) => idx1 == idx2,
            (CPEntry::InvokeDynamic(bootstrap1, nat1), CPEntry::InvokeDynamic(bootstrap2, nat2)) => bootstrap1 == bootstrap2 && nat1 == nat2,
            (_, _) => false,
        }
    }
}

impl Eq for CPEntry {}

#[derive(Debug)]
enum CPEntryType {
    Utf8,
    Integer,
    Float,
    Long,
    Double,
    Class,
    String,
    Fieldref,
    Methodref,
    InterfaceMethodref,
    NameAndType,
    MethodHandle,
    MethodType,
    InvokeDynamic,
    After8Byte,
}

impl Display for CPEntryType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn convert_jvm8_to_string(bytes: &[u8]) -> Result<String, ClassParseError> {
    Ok(jvm8::parse_jvm8(bytes)?.1)
}

fn write_jvm8(sink: &mut dyn Write, s: &str) -> io::Result<()> {
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        match c.into() {
            0 => extended_io::write_u16(sink, 0xC080)?,
            0x01..=0x7F => extended_io::write_u8(sink, c as u8)?,
            0x80..=0x7FF => { // At least 8 bits but less than 12.
                // 0x110ccccc 0x10cccccc
                let mut buf = [0; 2];
                c.encode_utf8(&mut buf);
                extended_io::write_byte_slice(sink, &buf[..])?
            },
            0x800..=0xFFFF => { // At least 12 bits but less than 17.
                // 0x1110cccc 0x10cccccc 0x10cccccc
                let mut buf = [0; 3];
                c.encode_utf8(&mut buf);
                extended_io::write_byte_slice(sink, &buf[..])?
            },
            0x1_0000..=0x10_FFFF => { // At least 17 bits but less than 22.
                // 0x11110ccc 0x10cccccc 0x10cccccc 0x10cccccc
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
                extended_io::write_byte_slice(sink, &buf[..])?
            },
            _ => break,
        }
    }
    Ok(())
}

fn read_utf8_cp_entry(src: &mut dyn Read) -> Result<CPEntry, ClassParseError> {
    let length = extended_io::read_u16(src)?.into();
    let bytes = extended_io::read_bytes(src, length)?;
    Ok(CPEntry::Utf8(convert_jvm8_to_string(&bytes)?))
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct JavaIdentifier(String);

impl JavaIdentifier {
    fn new(s: &str) -> Option<Self> {
        s.parse().ok()
    }
}

impl Display for JavaIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl NomParse for JavaIdentifier {
    fn nom_parse(s: &str) -> IResult<&str, Self> {
        comb::map(
            sequence::pair(
                // Can't use `nom::character::*::alpha1` because Java
                // identifiers are not required to be ASCII.
                // Can't match exactly one character because nom doesn't support
                // that in anything resembling a reasonable manner.
                bytes::take_while1(char::is_alphabetic), // \{alpha}+
                // As above
                bytes::take_while(char::is_alphanumeric), // \{alphanum}*
            ), // (\{alpha}+, \{alphanum}*)
            |(pre, rest)| {
                let mut ret = String::from(pre);
                ret.push_str(rest);
                JavaIdentifier(ret)
            },
        )(s)
    }
}

impl Borrow<str> for JavaIdentifier {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl_from_str_for_nom_parse!(JavaIdentifier);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PackageName(Vec<JavaIdentifier>);

impl PackageName {
    fn new(sections: Vec<JavaIdentifier>) -> PackageName {
        PackageName(sections)
    }

    fn sections(&self) -> &[JavaIdentifier] {
        &self.0[..]
    }

    fn to_internal_form(&self) -> String {
        self.sections().join("/")
    }

    fn to_source_form(&self) -> String {
        self.sections().join(".")
    }
}

impl NomParse for PackageName {
    fn nom_parse(s: &str) -> IResult<&str, PackageName> {
        comb::map(
            branch::alt((
                multi::many0(
                    sequence::terminated(
                        JavaIdentifier::nom_parse,
                        bytes::tag("/"),
                    )
                ),
                multi::many0(
                    sequence::terminated(
                        JavaIdentifier::nom_parse,
                        bytes::tag("."),
                    )
                ),
            )),
            PackageName::new,
        )(s)
    }
}

impl_from_str_for_nom_parse!(PackageName);

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnqualifiedClassName(JavaIdentifier);

impl Display for UnqualifiedClassName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl NomParse for UnqualifiedClassName {
    fn nom_parse(s: &str) -> IResult<&str, Self> {
        comb::map(JavaIdentifier::nom_parse, |x| UnqualifiedClassName(x))(s)
    }
}

impl_from_str_for_nom_parse!(UnqualifiedClassName);

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QualifiedClassName {
    ClassFile(PackageName, UnqualifiedClassName),
    Array(Either<JavaPrimitive, Box<QualifiedClassName>>),
}

impl QualifiedClassName {
    fn class_file(package: PackageName, class: UnqualifiedClassName)
            -> QualifiedClassName {

        QualifiedClassName::ClassFile(package, class)
    }

    fn primitive_array(el_type: JavaPrimitive) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Left(el_type))
    }

    fn class_array(el_type: QualifiedClassName) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Right(box el_type))
    }

    fn to_internal_form(&self) -> String {
        match self {
            QualifiedClassName::ClassFile(package, ucn) => {
                format!("{}/{}", package.to_internal_form(), ucn)
            }
            QualifiedClassName::Array(Either::Left(primitive)) => {
                format!("[{}", primitive.to_internal_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("[{}", qcn.to_internal_form())
            }
        }
    }

    fn to_source_form(&self) -> String {
        match self {
            QualifiedClassName::ClassFile(package, ucn) => {
                format!("{}.{}", package.to_source_form(), ucn)
            }
            QualifiedClassName::Array(Either::Left(primitive)) => {
                format!("{}[]", primitive.to_source_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("{}[]", qcn.to_source_form())
            }
        }
    }
}

impl NomParse for QualifiedClassName {
    fn nom_parse(s: &str) -> IResult<&str, QualifiedClassName> {
        branch::alt((
            comb::map(
                sequence::pair(
                    PackageName::nom_parse,
                    UnqualifiedClassName::nom_parse,
                ),
                |(package, name)| QualifiedClassName::ClassFile(package, name),
            ),
            comb::map(
                sequence::preceded(
                    bytes::tag("["),
                    branch::alt((
                        comb::map(
                            QualifiedClassName::nom_parse,
                            |x| Either::Right(box x)
                        ),
                        comb::map(
                            JavaPrimitive::nom_parse,
                            |x| Either::Left(x),
                        ),
                    )),
                ),
                QualifiedClassName::Array,
            ),
        ))(s)
    }
}

impl_from_str_for_nom_parse!(QualifiedClassName);

#[derive(Debug)]
enum CPAccessError {
    /// Attempted to access invalid index in the constant pool
    IndexOutOfBounds { request: u16, size: u16 },
    /// Attempted to access constant pool entry of incorrect type
    BadEntryType { request: u16, expected: CPEntryType, actual: CPEntryType },
    /// Failed to convert constant pool entry into a Rust type
    FailedTypeConversion { request: u16, source: CPEntryType, cause: Option<Box<dyn Error + Send + Sync>> },
    FailedParse(NomError),
    /// Attempted to add a new entry to a full constant pool.
    ConstantPoolFull,
}

impl Display for CPAccessError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CPAccessError::IndexOutOfBounds { request, size } => {
                write!(
                    f,
                    "Tried to access index {} in constant pool of size {}",
                    request,
                    size,
                )
            }
            CPAccessError::BadEntryType { request, expected, actual } => {
                write!(
                    f,
                    "Tried to get constant pool entry of type {} at index {} when actual type is {}",
                    expected,
                    request,
                    actual,
                )
            }
            CPAccessError::FailedTypeConversion {
                request,
                source,
                cause,
            } => {
                write!(
                    f,
                    "Tried to convert entry of type {} at index {} into another type but got error {:?}",
                    source,
                    request,
                    cause,
                )
            }
            CPAccessError::ConstantPoolFull => {
                write!(f, "Ran out of space in constant pool")
            }
            CPAccessError::FailedParse(e) => {
                write!(f, "Parse failure: {}", e)
            }
        }
    }
}

impl Error for CPAccessError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CPAccessError::FailedTypeConversion { cause, .. } => cause.as_ref().map(|b| &**b as &(dyn Error + 'static)),
            _ => None,
        }
    }
}

impl From<NomError> for CPAccessError {
    fn from(base: NomError) -> CPAccessError {
        CPAccessError::FailedParse(base)
    }
}

impl Into<io::Error> for CPAccessError {
    fn into(self) -> io::Error {
        mk_io_error(self)
    }
}

#[derive(Debug, Default)]
struct ConstantPool {
    pool: Vec<CPEntry>,
}

impl ConstantPool {
    fn new(pool: Vec<CPEntry>) -> ConstantPool {
        ConstantPool { pool }
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        extended_io::write_u16(sink, self.size() + 1)?;
        for i in 0..self.size() {
            self.pool[i as usize].write(sink)?;
        }
        Ok(())
    }

    fn size(&self) -> u16 {
        let len = self.pool.len();
        match len.try_into() {
            Ok(0xFFFF) | Err(_) => {
                panic!(
                    "Constant pool too large ({}). Maximum size is {}",
                    len,
                    0xFFFE,
                )
            },
            Ok(n) => n,
        }
    }

    fn check_bounds(&self, idx: u16) -> Result<(), CPAccessError> {
        // idx only out of bounds (to the right) if *greater* than self.size(),
        // since the constant pool is indexed from 1, not from 0.
        let size = self.size();
        if idx == 0 || size < idx {
            Err(CPAccessError::IndexOutOfBounds { request: idx, size })
        } else {
            Ok(())
        }
    }

    fn get(&self, idx: u16) -> Result<&CPEntry, CPAccessError> {
        self.check_bounds(idx)
            .and_then(move |_| Ok(&self.pool[idx as usize - 1]))
    }

    fn get_string(&self, idx: u16) -> Result<&str, CPAccessError> {
        self.check_bounds(idx).and_then(move |_| self.get(idx))
            .and_then(move |entry| {
                match entry {
                    &CPEntry::Utf8(ref s) => Ok(s.as_ref()),
                    entry => {
                        let e = CPAccessError::BadEntryType {
                            request: idx,
                            expected: CPEntryType::Utf8,
                            actual: entry.r#type(),
                        };
                        Err(e)
                    }
                }
            })
    }

    fn get_owned_string(&self, idx: u16) -> Result<String, CPAccessError> {
        self.get_string(idx).map(str::to_string)
    }

    fn get_class_name(&self, idx: u16)
            -> Result<QualifiedClassName, CPAccessError> {

        match self.get(idx)? {
            &CPEntry::Class(idx) => {
                self.get_string(idx)?.parse().map_err(|_| {
                    CPAccessError::FailedTypeConversion {
                        request: idx,
                        source: CPEntryType::Utf8,
                        cause: None,
                    }
                })
            },
            entry => {
                let err = CPAccessError::BadEntryType {
                    request: idx,
                    expected: CPEntryType::Class,
                    actual: entry.r#type(),
                };
                Err(err)
            },
        }
    }

    fn get_type(&self, idx: u16) -> Result<JavaType, CPAccessError> {
        match self.get(idx)? {
            &CPEntry::Utf8(ref s) => {
                JavaType::from_str(s)
                    .or_else(move |_| {
                        let err = CPAccessError::FailedTypeConversion {
                            request: idx,
                            source: CPEntryType::Utf8,
                            cause: None,
                        };
                        Err(err)
                    })
            }
            entry => {
                let err = CPAccessError::BadEntryType {
                    request: idx,
                    expected: CPEntryType::Utf8,
                    actual: entry.r#type(),
                };
                Err(err)
            }
        }
    }

    fn index_of(&self, entry: &CPEntry) -> Option<u16> {
        for i in 1..=self.size() {
            match self.get(i) {
                Ok(e) if entry == e => return Some(i),
                Ok(_) => {},
                Err(_) => unreachable!(),
            }
        }
        None
    }

    fn add(&mut self, entry: CPEntry) -> Result<u16, CPAccessError> {
        match (self.index_of(&entry), self.size()) {
            (None, 0xFFFF) => unreachable!("Should never exceed 0xFFFE entries in pool"),
            (None, 0xFFFE) => Err(CPAccessError::ConstantPoolFull),
            (None, size) => {
                let wide = match entry {
                    CPEntry::Long(_) | CPEntry::Double(_) => true,
                    _ => false,
                };
                self.pool.push(entry);
                if wide {
                    self.pool.push(CPEntry::After8Byte);
                }
                Ok(size + 1)
            }
            (Some(idx), _) => Ok(idx),
        }
    }

    fn add_string(&mut self, s: String) -> Result<u16, CPAccessError> {
        self.add(CPEntry::Utf8(s))
    }

    fn add_type(&mut self, r#type: JavaType) -> Result<u16, CPAccessError> {
        self.add_string(r#type.to_internal_form())
    }

    fn add_class_name(&mut self, name: QualifiedClassName)
            -> Result<u16, CPAccessError> {

        self.add_string(name.to_internal_form())
    }

    fn add_interfaces(&mut self, interfaces: Vec<QualifiedClassName>)
            -> Result<Vec<u16>, CPAccessError> {

        interfaces.into_iter().map(|interface| self.add_class_name(interface))
            .collect()
    }

    fn add_field(&mut self, field: JavaField)
            -> Result<RawField, CPAccessError> {

        field.into_raw(self)
    }

    fn add_fields(&mut self, fields: Vec<JavaField>)
            -> Result<Vec<RawField>, CPAccessError> {

        fields.into_iter().map(|field| self.add_field(field)).collect()
    }

    fn add_method(&mut self, method: JavaMethod)
            -> Result<RawMethod, CPAccessError> {

        method.into_raw(self)
    }

    fn add_methods(&mut self, methods: Vec<JavaMethod>)
            -> Result<Vec<RawMethod>, CPAccessError> {

        methods.into_iter().map(|method| self.add_method(method)).collect()
    }

    fn add_attribute(&mut self, attribute: JavaAttribute)
            -> Result<RawAttribute, CPAccessError> {

        attribute.into_raw(self)
    }

    fn add_attributes(&mut self, attributes: Vec<JavaAttribute>)
            -> Result<Vec<RawAttribute>, CPAccessError> {

        attributes.into_iter().map(|attribute| self.add_attribute(attribute))
            .collect()
    }
}

fn read_constant_pool(src: &mut dyn Read)
        -> Result<ConstantPool, ClassParseError> {

    let len = extended_io::read_u16(src)?;
    let mut ret = ConstantPool { pool: Vec::with_capacity(len as usize - 1) };
    for _ in 0..(len - 1) {
        ret.pool.push(CPEntry::read(src)?);
    }
    Ok(ret)
}

pub trait AccessFlagged {
    /// The class, field, or method is declared public: it may be accessed
    /// anywhere the declaring class or class file can be accessed.
    fn is_public(&self) -> bool; // 0x0001
    /// The class, field, or method is declared private: it may not be accessed
    /// from outside of the class where it is declared; it may not be a top-
    /// level class.
    fn is_private(&self) -> bool; // 0x0002
    /// The class, field, or method is declared protected: it may not be
    /// accessed by any class which is neither in the same package nor a
    /// subclass of the declaring class; it may not be a top-level class.
    fn is_protected(&self) -> bool; // 0x0004
    /// The class, field, or method does not have an explicit visibility: it may
    /// not be accessed outside of the package.
    fn is_package_protected(&self) -> bool {
        !self.is_public() && !self.is_private() && !self.is_protected()
    }
    /// The class, field, or method is declared static. A static class must not
    /// be top-level.
    fn is_static(&self) -> bool; // 0x0008
    /// The class, field, or method is declared final.
    fn is_final(&self) -> bool; // 0x0010
    /// The JVM should ignore the class name in all uses of invokespecial in
    /// this class file. Beginning with 7u13, Oracle's JVM treated all class
    /// files as if this attribute were present, regardless of whether it
    /// actually is.
    fn is_super_special(&self) -> bool; // 0x0020
    /// The method is declared synchronized. A class or field cannot be
    /// synchronized.
    fn is_synchronized(&self) -> bool; // 0x0020
    /// The field is declared volatile: it must not be cached. A class or method
    /// cannot be volatile.
    fn is_volatile(&self) -> bool; // 0x0040
    /// The method is a bridge method. A bridge method is generated when a
    /// generic method is "overridden" by a method with a different erasure.
    /// For example, given the following classes, the compiler erases
    /// Node<T>.setData(T) to Node.setData(Object) and generates a method
    /// MyNode.setData(Object) which calls MyNode.setData(Integer) with the
    /// proper casts.
    ///
    /// ```
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
    /// `MyNode.setData(Object)` is generated as a delegate to
    /// `MyNode.setData(Integer)` so that calling setData on a MyNode object
    /// with a non-Integer argument will produce an exception instead of
    /// behaving badly in silence.
    /// A class or field cannot be a bridge.
    fn is_bridge(&self) -> bool; // 0x0040
    /// A transient field is not serialized by the default Java serialization
    /// functions. A transient field should be re-initialized on deserialization
    /// by either the no-args constructor (if it is not dependent on the values
    /// of non-transient fields) or a custom deserializer with signature and
    /// exceptions "private void readObject(ObjectInputStream) throws
    /// IOException, ClassNotFoundException".
    fn is_transient(&self) -> bool; // 0x0080
    /// The method has variable arity. A class or field cannot be varargs.
    fn is_varargs(&self) -> bool; // 0x0080
    /// The method was not implemented in Java. A class or field cannot be
    /// native.
    fn is_native(&self) -> bool; // 0x0100
    /// The class is an interface. A method or field cannot be an interface.
    fn is_interface(&self) -> bool; // 0x0200
    /// The class or method is abstract. An abstract class cannot be instantiated
    /// directly. An abstract method does not have a body and must be implemented
    /// by any instantiable subclass of its parent class. A field cannot be
    /// abstract.
    fn is_abstract(&self) -> bool; // 0x0400
    /// Require all floating point operations in the method or class to remain
    /// adherent to IEEE 754 in all intermediate steps. Default Java behavior is
    /// to allow extended exponent range.
    fn is_strict(&self) -> bool; // 0x0800
    /// The class, method, or field is not present in the associated source.
    fn is_synthetic(&self) -> bool; // 0x1000
    /// The class is an annotation type: declared as "@interface".
    fn is_annotation(&self) -> bool; // 0x2000
    /// The class or field is an enum or enum constant.
    fn is_enum(&self) -> bool; // 0x4000
}

pub trait RawAttributeInfo {}

enum RawAttribute {
    GenericAttribute { name_idx: u16, info: Vec<u8> },
}

impl RawAttribute {
    fn write(self, sink: &mut dyn Write) -> io::Result<()> {
        match self {
            RawAttribute::GenericAttribute { name_idx, info } => {
                extended_io::write_u16(sink, name_idx)?;
                let info_len = info.len().try_into().map_err(mk_io_error)?;
                extended_io::write_u32(sink, info_len)?;
                extended_io::write_byte_slice(sink, &info[..])?;
                Ok(())
            }
        }
    }
}

pub enum JavaAttribute {
    GenericAttribute { name: String, info: Vec<u8> },
}

impl JavaAttribute {
    fn read(src: &mut dyn Read, pool: &ConstantPool)
            -> io::Result<JavaAttribute> {

        let name_idx = extended_io::read_u16(src)?;
        match pool.get_string(name_idx) {
            Err(e) => Err(e.into()),
            // TODO: implement specializations for known attribute types
            Ok(name) => {
                let name = name.to_string();
                let info_length = extended_io::read_u32(src)?.into();
                let info = extended_io::read_bytes(src, info_length)?;
                Ok(JavaAttribute::GenericAttribute { name, info })
            }
        }
    }

    pub fn name(&self) -> &str {
        match self {
            JavaAttribute::GenericAttribute { name, .. } => name.as_ref(),
        }
    }

    pub fn info_bytes(&self) -> Option<&[u8]> {
        match self {
            JavaAttribute::GenericAttribute { info, .. } => Some(&info[..])
        }
    }

    fn into_raw(self, pool: &mut ConstantPool)
            -> Result<RawAttribute, CPAccessError> {

        match self {
            JavaAttribute::GenericAttribute { name, info } => {
                let name_idx = pool.add_string(name)?;
                Ok(RawAttribute::GenericAttribute { name_idx, info })
            }
        }
    }
}

fn read_attributes(src: &mut dyn Read, pool: &ConstantPool)
        -> io::Result<Vec<JavaAttribute>> {

    let num_attributes = extended_io::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_attributes.into());
    for _ in 0..num_attributes {
        ret.push(JavaAttribute::read(src, pool)?);
    }
    Ok(ret)
}

struct RawField {
    access_flags: u16,
    name_idx: u16,
    type_idx: u16,
    attributes: Vec<RawAttribute>,
}

impl RawField {
    fn write(self, sink: &mut dyn Write) -> io::Result<()> {
        extended_io::write_u16(sink, self.access_flags)?;
        extended_io::write_u16(sink, self.name_idx)?;
        extended_io::write_u16(sink, self.type_idx)?;
        extended_io::write_u16(sink, self.attributes.len().try_into().map_err(mk_io_error)?)?;
        for attribute in self.attributes {
            attribute.write(sink)?;
        }
        Ok(())
    }
}

pub struct JavaField {
    access_flags: u16,
    name: String,
    r#type: JavaType,
    attributes: Vec<JavaAttribute>,
}

impl JavaField {
    fn read(src: &mut dyn Read, pool: &ConstantPool)
            -> io::Result<JavaField> {

        let access_flags = extended_io::read_u16(src)?;

        let name_idx = extended_io::read_u16(src)?;
        let name = pool.get_owned_string(name_idx).map_err(mk_io_error)?;

        let descriptor_idx = extended_io::read_u16(src)?;
        let r#type = pool.get_type(descriptor_idx).map_err(mk_io_error)?;

        let attributes = read_attributes(src, pool).map_err(mk_io_error)?;

        Ok(JavaField { access_flags, name, r#type, attributes})
    }

    pub fn new(
            access_flags: u16,
            name: String,
            r#type: JavaType,
            attributes: Vec<JavaAttribute>)
            -> JavaField {

        JavaField { access_flags, name, r#type, attributes }
    }

    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    pub fn r#type(&self) -> &JavaType {
        &self.r#type
    }

    pub fn attributes(&self) -> &[JavaAttribute] {
        &self.attributes[..]
    }

    fn into_raw(self, pool: &mut ConstantPool)
            -> Result<RawField, CPAccessError> {

        let access_flags = self.access_flags;
        let name_idx = pool.add_string(self.name)?;
        let type_idx = pool.add_type(self.r#type)?;
        let attributes = pool.add_attributes(self.attributes)?;
        Ok(RawField { access_flags, name_idx, type_idx, attributes })
    }
}

fn read_fields(src: &mut dyn Read, pool: &ConstantPool)
        -> io::Result<Vec<JavaField>> {

    let num_fields = extended_io::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_fields.into());
    for _ in 0..num_fields {
        ret.push(JavaField::read(src, pool)?);
    }
    Ok(ret)
}

impl AccessFlagged for JavaField {
    fn is_public(&self) -> bool {
        self.access_flags & 0x0001 != 0
    }

    fn is_private(&self) -> bool {
        self.access_flags & 0x0002 != 0
    }

    fn is_protected(&self) -> bool {
        self.access_flags & 0x0004 != 0
    }

    fn is_package_protected(&self) -> bool {
        self.access_flags & 0x0007 == 0
    }

    fn is_static(&self) -> bool {
        self.access_flags & 0x0008 != 0
    }

    fn is_final(&self) -> bool {
        self.access_flags & 0x0010 != 0
    }

    fn is_super_special(&self) -> bool {
        false
    }

    fn is_synchronized(&self) -> bool {
        false
    }

    fn is_volatile(&self) -> bool {
        self.access_flags & 0x0040 != 0
    }

    fn is_bridge(&self) -> bool {
        false
    }

    fn is_transient(&self) -> bool {
        self.access_flags & 0x0080 != 0
    }

    fn is_varargs(&self) -> bool {
        false
    }

    fn is_native(&self) -> bool {
        false
    }

    fn is_interface(&self) -> bool {
        false
    }

    fn is_abstract(&self) -> bool {
        false
    }

    fn is_strict(&self) -> bool {
        false
    }

    fn is_synthetic(&self) -> bool {
        self.access_flags & 0x1000 != 0
    }

    fn is_annotation(&self) -> bool {
        false
    }

    fn is_enum(&self) -> bool {
        self.access_flags & 0x4000 != 0
    }
}

struct RawMethod {
    access_flags: u16,
    name_idx: u16,
    type_idx: u16,
    attributes: Vec<RawAttribute>,
}

impl RawMethod {
    fn write(self, sink: &mut dyn Write) -> io::Result<()> {
        extended_io::write_u16(sink, self.access_flags)?;
        extended_io::write_u16(sink, self.name_idx)?;
        extended_io::write_u16(sink, self.type_idx)?;
        extended_io::write_u16(sink, self.attributes.len().try_into().map_err(mk_io_error)?)?;
        for attribute in self.attributes {
            attribute.write(sink)?;
        }
        Ok(())
    }
}

fn read_methods(src: &mut dyn Read, pool: &ConstantPool)
        -> Result<Vec<JavaMethod>, ClassParseError> {

    let num_methods = extended_io::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_methods.into());
    for _ in 0..num_methods {
        ret.push(JavaMethod::read(src, pool)?);
    }
    Ok(ret)
}

pub struct JavaMethod {
    access_flags: u16,
    name: String,
    r#type: JavaType,
    attributes: Vec<JavaAttribute>,
}

impl JavaMethod {
    fn read(src: &mut dyn Read, pool: &ConstantPool)
            -> Result<JavaMethod, ClassParseError> {

        let access_flags = extended_io::read_u16(src)?;
        let name = pool.get_owned_string(extended_io::read_u16(src)?)?;
        // TODO: implement check that `type` is a function type
        let r#type = pool.get_type(extended_io::read_u16(src)?)?;
        let attributes = read_attributes(src, pool)?;
        Ok(JavaMethod { access_flags, name, r#type, attributes })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn r#type(&self) -> &JavaType {
        &self.r#type
    }

    pub fn attributes(&self) -> &[JavaAttribute] {

        &self.attributes[..]
    }

    fn into_raw(self, pool: &mut ConstantPool)
            -> Result<RawMethod, CPAccessError> {

        let access_flags = self.access_flags;
        let name_idx = pool.add_string(self.name)?;
        let type_idx = pool.add_type(self.r#type)?;
        let attributes = pool.add_attributes(self.attributes)?;
        Ok(RawMethod { access_flags, name_idx, type_idx, attributes })
    }
}

impl AccessFlagged for JavaMethod {
    fn is_public(&self) -> bool {
        self.access_flags & 0x0001 != 0
    }

    fn is_private(&self) -> bool {
        self.access_flags & 0x0002 != 0
    }

    fn is_protected(&self) -> bool {
        self.access_flags & 0x0004 != 0
    }

    fn is_package_protected(&self) -> bool {
        !self.is_public() && !self.is_private() && !self.is_protected()
    }

    fn is_static(&self) -> bool {
        self.access_flags & 0x0008 != 0
    }

    fn is_final(&self) -> bool {
        self.access_flags & 0x0010 != 0
    }

    fn is_super_special(&self) -> bool {
        false
    }

    fn is_synchronized(&self) -> bool {
        self.access_flags & 0x0020 != 0
    }

    fn is_volatile(&self) -> bool {
        false
    }

    fn is_bridge(&self) -> bool {
        self.access_flags & 0x0040 != 0
    }

    fn is_transient(&self) -> bool {
        false
    }

    fn is_varargs(&self) -> bool {
        self.access_flags & 0x0080 != 0
    }

    fn is_native(&self) -> bool {
        self.access_flags & 0x0100 != 0
    }

    fn is_interface(&self) -> bool {
        false
    }

    fn is_abstract(&self) -> bool {
        self.access_flags & 0x0400 != 0
    }

    fn is_strict(&self) -> bool {
        self.access_flags & 0x0800 != 0
    }

    fn is_synthetic(&self) -> bool {
        self.access_flags & 0x1000 != 0
    }

    fn is_annotation(&self) -> bool {
        false
    }

    fn is_enum(&self) -> bool {
        false
    }
}

#[derive(Copy, Clone)]
pub struct ClassFileVersion(u16, u16);

impl ClassFileVersion {
    pub const fn new(major_version: u16, minor_version: u16)
            -> ClassFileVersion {

        ClassFileVersion(major_version, minor_version)
    }

    pub fn major_version(&self) -> u16 {
        self.0
    }

    pub fn minor_version(&self) -> u16 {
        self.1
    }
}

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

fn read_interfaces(src: &mut dyn Read, constant_pool: &ConstantPool)
        -> Result<Vec<QualifiedClassName>, ClassParseError> {

    let num_interfaces = extended_io::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_interfaces as usize);
    for _ in 0..num_interfaces {
        let class_idx = extended_io::read_u16(src)?;
        ret.push(constant_pool.get_class_name(class_idx)?);
    }
    Ok(ret)
}

impl JavaClass {
    pub fn new(
            version: ClassFileVersion,
            access_flags: u16,
            this_class: QualifiedClassName,
            super_class: QualifiedClassName,
            interfaces: Vec<QualifiedClassName>,
            fields: Vec<JavaField>,
            methods: Vec<JavaMethod>,
            attributes: Vec<JavaAttribute>)
            -> JavaClass {

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

    pub fn read(src: &mut dyn Read) -> Result<JavaClass, ClassParseError> {
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
        match extended_io::read_u32(src)? {
            0xCAFE_BABE => {},
            n => return Err(ClassParseError::InvalidMagicNumber {actual: n}),
        }
        let version = {
            let minor_version = extended_io::read_u16(src)?;
            let major_version = extended_io::read_u16(src)?;
            ClassFileVersion::new(major_version, minor_version)
        };
        let constant_pool = read_constant_pool(src)?;
        let access_flags = extended_io::read_u16(src)?;
        let this_class = {
            let this_class_idx = extended_io::read_u16(src)?;
            constant_pool.get_class_name(this_class_idx)?.into()
        };
        let super_class = {
            let super_class_idx = extended_io::read_u16(src)?;
            constant_pool.get_class_name(super_class_idx)?.into()
        };
        let interfaces = read_interfaces(src, &constant_pool)?;
        let fields = read_fields(src, &constant_pool)?;
        let methods = read_methods(src, &constant_pool)?;
        let attributes = read_attributes(src, &constant_pool)?;
        let ret = JavaClass::new(
            version,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes
        );
        Ok(ret)
    }

    pub fn write(self, sink: &mut dyn Write) -> io::Result<()> {
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
        let mut pool = ConstantPool::new(Vec::new());
        // TODO: check if `?` uses `Into` instead of `From` yet
        let this_class_idx = pool.add_class_name(self.this_class)
            .map_err(mk_io_error)?;
        let super_class_idx = pool.add_class_name(self.super_class)
            .map_err(mk_io_error)?;
        let interface_idxs = pool.add_interfaces(self.interfaces)
            .map_err(mk_io_error)?;
        let raw_fields = pool.add_fields(self.fields).map_err(mk_io_error)?;
        let raw_methods = pool.add_methods(self.methods)
            .map_err(mk_io_error)?;
        let raw_attributes = pool.add_attributes(self.attributes)
            .map_err(mk_io_error)?;

        extended_io::write_u32(sink, 0xCAFE_BABE)?;
        extended_io::write_u16(sink, self.version.minor_version())?;
        extended_io::write_u16(sink, self.version.major_version())?;
        pool.write(sink)?;
        extended_io::write_u16(sink, self.access_flags)?;
        extended_io::write_u16(sink, this_class_idx)?;
        extended_io::write_u16(sink, super_class_idx)?;
        extended_io::write_u16(
            sink,
            interface_idxs.len().try_into().map_err(mk_io_error)?,
        )?;
        for interface_idx in interface_idxs {
            extended_io::write_u16(sink, interface_idx)?;
        }
        extended_io::write_u16(
            sink,
            raw_fields.len().try_into().map_err(mk_io_error)?,
        )?;
        for raw_field in raw_fields {
            raw_field.write(sink)?;
        }
        extended_io::write_u16(
            sink,
            raw_methods.len().try_into().map_err(mk_io_error)?,
        )?;
        for raw_method in raw_methods {
            raw_method.write(sink)?;
        }
        extended_io::write_u16(
            sink,
            raw_attributes.len().try_into().map_err(mk_io_error)?,
        )?;
        for raw_attribute in raw_attributes {
            raw_attribute.write(sink)?;
        }
        Ok(())
    }

    pub fn get_class_file_version(&self) -> ClassFileVersion {
        self.version
    }

    pub fn get_name(&self) -> &QualifiedClassName {
        &self.this_class
    }

    pub fn get_superclass_name(&self) -> &QualifiedClassName {
        &self.super_class
    }

    pub fn get_interface_names(&self) -> &[QualifiedClassName] {
        &self.interfaces[..]
    }

    pub fn fields(&self) -> &[JavaField] {
        &self.fields[..]
    }

    pub fn methods(&self) -> &[JavaMethod] {
        &self.methods[..]
    }

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
        let result = JavaType::nom_parse(&"");
        let _ = result.expect_err("JavaType parser should fail on empty string");
    }

    #[test]
    fn java_type_parses_byte() {
        let src = "B";
        let target = "JavaType::Byte";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Byte => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_char() {
        let src = "C";
        let target = "JavaType::Char";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Char => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_double() {
        let src = "D";
        let target = "JavaType::Double";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Double => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_float() {
        let src = "F";
        let target = "JavaType::Float";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Float => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_int() {
        let src = "I";
        let target = "JavaType::Int";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Int => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_long() {
        let src = "J";
        let target = "JavaType::Primitive(JavaPrimitive::Long)";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Primitive(JavaPrimitive::Long) => {},
            r#type => panic!("Expected {}, found {:?}", target, r#type),
        }
    }

    #[test]
    fn java_type_parses_class_name_default_package() {
        let src = "LClass;";
        let target = r#"JavaType::Class(QualifiedClassName::ClassFile([], UnqualifiedClassName(JavaIdentifier("Class"))))"#;
        let result = parse(&src).expect(&format!("{} should be {}", src, target)).1;
        let panic_msg = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Class(
                QualifiedClassName::ClassFile(
                    vec![],
                    UnqualifiedClassName(JavaIdentifier(class_name))
                )
            ) => {
                match class_name.as_ref() {
                    "Class" => {},
                    r#type => panic!(panic_msg),
                }
            },
            _ => panic!(panic_msg),
        }
    }

    #[test]
    fn parses_class_name_Object() {
        let src = "Ljava/lang/Object;";
        let target = r#"Class("java/lang/Object")"#;
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Class(class_name) => {
                match class_name.as_ref() {
                    "java/lang/Object" => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    #[test]
    fn parses_short() {
        let src = "S";
        let target = "Short";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Short => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    #[test]
    fn parses_bool() {
        let src = "Z";
        let target = "Bool";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Bool => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    #[test]
    fn parses_array_int() {
        let src = "[I";
        let target = "Array(Int)";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Array(box JavaType::Int) => {},
            result => panic!(format!("Expected {}, found {:?}", target, result)),
        }
    }

    #[test]
    fn parses_array_array_bool() {
        let src = "[[Z";
        let target = "Array(Array(Int))";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Array(box JavaType::Array(box JavaType::Bool)) => {},
            result => panic!(format!("Expected {}, found {:?}", target, result)),
        }
    }

    #[test]
    fn parses_func_thunk() {
        let src = "()V";
        let target = "Method([], Void)";
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(args, box JavaType::Void) => {
                match &(*args)[..] {
                    &[] => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    #[test]
    fn parses_func_int_void() {
        let src = "(I)V";
        let target = "Method([Int], Void)";
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(args, box JavaType::Void) => {
                match &(*args)[..] {
                    &[JavaType::Int] => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    #[test]
    fn parses_func_array_int_int_Object() {
        let src = "([II)Ljava/lang/Object;";
        let target = r#"Method([Array(Int), Int], Class("java/lang/Object")"#;
        let result = parse(&src)
            .expect(&format!(r#"{} should be {}"#, src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(ref args, box JavaType::Class(ref class_name)) => {
                match (&(*args)[..], class_name.as_ref()) {
                    (&[JavaType::Array(box JavaType::Int), JavaType::Int], "java/lang/Object") => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }
}
