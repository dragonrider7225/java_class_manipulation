#![feature(box_syntax)]

use sized_io::{read_u8, read_u16, read_u32};

use std::io::Read;
use std::str::FromStr;

mod sized_io {
    use std::io::Read;

    #[derive(Debug)]
    pub enum ReadError {
        /// Tried to read arg bytes but found EOF.
        NotEnoughBytes(usize),
    }

    pub fn read_u8(src: &mut Read) -> Result<u8, ReadError> {
        let mut buf = [0; 1];
        if let Err(e) = src.read_exact(&mut buf) {
            Err(ReadError::NotEnoughBytes(1))
        } else {
            Ok(buf[0])
        }
    }
    
    pub fn read_u16(src: &mut Read) -> Result<u16, ReadError> {
        let mut buf = [0; 2];
        if let Err(e) = src.read_exact(&mut buf) {
            Err(ReadError::NotEnoughBytes(2))
        } else {
            Ok(((buf[0] as u16) << 8) + (buf[1] as u16))
        }
    }
    
    pub fn read_u32(src: &mut Read) -> Result<u32, ReadError> {
        let mut buf = [0; 4];
        if let Err(e) = src.read_exact(&mut buf) {
            Err(ReadError::NotEnoughBytes(4))
        } else {
            Ok(((buf[0] as u32) << 24) + ((buf[1] as u32) << 16) + ((buf[2] as u32) << 8) + (buf[3] as u32))
        }
    }

    pub fn read_bytes(src: &mut Read, length: usize)
            -> Result<Vec<u8>, ReadError> {

        let mut handle = src.take(length as u64);
        let mut buf = vec![];
        if let Err(_) = handle.read_to_end(&mut buf) {
            Err(ReadError::NotEnoughBytes(length))
        } else {
            Ok(buf)
        }
    }
}

#[derive(Debug)]
pub enum TypeDescProblem {
    /// The generic "A type was expected" problem. If reading the return type
    /// for a function, use NoReturnType instead.
    NoType,
    /// The "Missing return type for a function type" problem.
    NoReturnType,
    /// The "Found character after end of type description" problem.
    OverlyLong,
    /// The "Function type is used as argument type or return type for another
    /// function type" problem.
    NestedFunctionType,
    /// The "Character doesn't begin a short type name" problem.
    ExpectedType(u8),
    /// The "Ran out of characters while reading a class name" problem.
    MissingTerminator(String),
}

#[derive(Debug)]
pub enum ClassParseError {
    /// arg is a description of the unimplemented section
    NotImplemented(String),
    /// arg is the four bytes from the class file that are expected to be the
    /// magic number but aren't.
    InvalidMagicNumber(u32),
    /// arg is the byte that is expected to be the tag for an entry in the
    /// constant pool but is not a valid tag.
    InvalidConstantPoolEntryTag(u8),
    /// first arg is a description of the expected byte sequence(s), second arg
    /// is the actual byte sequence.
    InvalidByteSequence(String, Vec<u8>),
    /// first arg is the kind of problem with the type description, second arg
    /// is the malformed type description, if available.
    InvalidTypeDesc(TypeDescProblem, Option<String>),
    /// A wrapper around a sized_io::ReadError.
    ReadError(sized_io::ReadError),
}

impl From<sized_io::ReadError> for ClassParseError {
    fn from(base: sized_io::ReadError) -> ClassParseError {
        ClassParseError::ReadError(base)
    }
}

#[derive(Debug)]
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

impl ReferenceKind {
    pub fn from_u8(kind: u8) -> Result<ReferenceKind, ClassParseError> {
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
                ClassParseError::InvalidByteSequence(
                    String::from("Expected reference kind"),
                    vec![n])),
        }
    }

    pub fn to_u8(&self) -> u8 {
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

#[derive(Debug, PartialEq, Eq)]
pub enum JavaType {
    Bool,
    Char,
    Byte,
    Short,
    Int,
    Long,
    Float,
    Double,
    Array(Box<JavaType>),
    Class(String),
    Method(Box<Vec<JavaType>>, Box<JavaType>),
}

impl JavaType {
    fn read_prefix(s: &[u8], is_func: bool)
            -> Result<(JavaType, usize), ClassParseError> {

        if s.len() == 0 {
            return Err(ClassParseError::InvalidTypeDesc(
                    TypeDescProblem::NoType, None));
        }
        match s[0] {
            b'B' => Ok((JavaType::Byte, 1)),
            b'C' => Ok((JavaType::Char, 1)),
            b'D' => Ok((JavaType::Double, 1)),
            b'F' => Ok((JavaType::Float, 1)),
            b'I' => Ok((JavaType::Int, 1)),
            b'J' => Ok((JavaType::Long, 1)),
            // take_while (on a ref) eats ALL elements of the source iterator up
            // to and including the first element that does not match the
            // predicate, if such an element exists.
            b'L' => {
                let mut chars_read = 1;
                let mut ret = String::from("");
                loop {
                    match s[chars_read] {
                        b';' => {
                            chars_read += 1;
                            break;
                        },
                        c => {
                            chars_read += 1;
                            ret.push(char::from(c));
                        },
                    }
                }
                // TODO: implement checking on ret for valid package and class
                // identifiers.
                // identifier: alpha alphanum*
                // alpha: filter(char::is_alphabetic, Unicode)
                // alphanum: filter(char::is_alphanum, Unicode)
                // absolute_class_id: identifier ('/' identifier)*
                Ok((JavaType::Class(ret), chars_read))
            },
            b'S' => Ok((JavaType::Short, 1)),
            b'Z' => Ok((JavaType::Bool, 1)),
            b'[' => {
                let (el_type, el_width) = JavaType::read_prefix(s, is_func)?;
                Ok((JavaType::Array(box el_type), el_width + 1))
            },
            b'(' => {
                if is_func {
                    return Err(ClassParseError::InvalidTypeDesc(
                            TypeDescProblem::NestedFunctionType,
                            None));
                }
                // Skip over the opening parenthesis of the function type
                // definition
                let mut chars_read = 1;
                let mut arg_types = vec![];
                while s[chars_read] != b')' {
                    let (arg_type, arg_width) = JavaType::read_prefix(
                            &s[chars_read..], true)?;
                    arg_types.push(arg_type);
                    chars_read += arg_width;
                }
                // Skip over the closing parenthesis of the function type
                // definition
                chars_read += 1;
                match JavaType::read_prefix(&s[chars_read..], true) {
                    Err(ClassParseError::InvalidTypeDesc(
                            TypeDescProblem::NoType, s)) => Err(
                                    ClassParseError::InvalidTypeDesc(
                                            TypeDescProblem::NoReturnType, s)),
                    Err(e) => Err(e),
                    Ok((ret_type, ret_width)) => Ok((
                            JavaType::Method(box arg_types, box ret_type),
                            chars_read + ret_width)),
                }
            },
            c => Err(ClassParseError::InvalidTypeDesc(
                    TypeDescProblem::ExpectedType(c), None)),
        }
    }
}

impl FromStr for JavaType {
    type Err = ClassParseError;

    fn from_str(s: &str) -> Result<JavaType, ClassParseError> {
        match JavaType::read_prefix(s.as_bytes(), false) {
            Err(ClassParseError::InvalidTypeDesc(p, None)) => Err(
                    ClassParseError::InvalidTypeDesc(p, Some(String::from(s)))),
            Err(e) => Err(e),
            Ok((ret, ret_width)) => {
                if ret_width != s.len() {
                    Err(ClassParseError::InvalidTypeDesc(
                            TypeDescProblem::OverlyLong, Some(String::from(s))))
                } else {
                    Ok(ret)
                }
            },
        }
    }
}

#[derive(Debug)]
pub enum CPEntry {
    /**
     * arg is modified UTF-8 string according to the following rules:
     * Positive code points less than 0x80 are represented by 0b0ccccccc where
     * the cs represent the least significant bits of the source code point.
     * Other code points less than 0x800 are represented by 0b110ccccc
     * 0b10cccccc where the cs are as previously.
     * Other code points less than 0x1_0000 are represented by 0b1110cccc
     * 0b10cccccc 0b10cccccc where the cs are as previously.
     * Other code points are converted to their UTF-16 encodings and represented
     * by the concatenation of the encodings of the surrogates. This has the
     * effect of producing the bytes 0b11101101 0b1010dddd 0b10cccccc 0b11101101
     * 0b1011cccc 0b10cccccc where the cs are as previously and the ds are what
     * remains when the code point is shifted right by 16 bits then reduced by
     * 1.
     */
    Utf8(String),
    /// A type for a field or method. Stored in the class file as Utf8.
    Type(JavaType),
    /// arg is the bytes of the (two's-complement) integer in big-endian order.
    Integer(u32),
    /// arg is bytes of the IEEE 754 single-precision floating-point number in
    /// big-endian order.
    Float(u32),
    /// first arg is high bits, second arg is low bits.
    Long(u32, u32),
    /// same as above but IEEE 754 double-precision.
    Double(u32, u32),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains
    /// the fully-qualified name of a class.
    Class(u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains
    /// the contents of the string.
    String(u16),
    /**
     * first arg is 1-based index of the Class entry for the class that owns
     * the referenced field, second arg is 1-based index of the NameAndType
     * entry of the referenced field.
     */
    Fieldref(u16, u16),
    /// Same as Fieldref except that first arg must not be index of an interface
    /// and second arg must be a method.
    Methodref(u16, u16),
    /// Same as Methodref except that first arg must be index of an interface.
    InterfaceMethodref(u16, u16),
    /**
     * first arg is 1-based index of a Utf8 entry in the constant pool which
     * contains the name of the reference, second arg is 1-based index of a
     * Utf8 entry in the constant pool which contains the type descriptor of
     * the reference.
     */
    NameAndType(u16, u16),
    /// first arg is reference kind, second arg is 1-based index of the
    /// {Field,Method,InterfaceMethod}ref in the constant pool to interact with.
    MethodHandle(ReferenceKind, u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains
    /// the method descriptor.
    MethodType(u16),
    /**
     * first arg is 0-based index into bootstrap methods array in the
     * attributes table, second arg is 1-based index of a NameAndType entry
     * representing the referenced method.
     */
    InvokeDynamic(u16, u16),
}

fn convert_jvm8_to_string(bytes: &[u8]) -> Result<String, ClassParseError> {
    let bad_bytes_msg = "Expected 0x0ccccccc, 0x110ccccc 0x10cccccc, or 0x1110cccc 0x10cccccc 0x10cccccc";
    let mut ret = String::with_capacity(bytes.len());
    // A JVM-8 string is never shorter than the equivalent UTF-8 string
    let mut src_iter = bytes.iter().map(|&c| c as u32);
    while let Some(c0) = src_iter.next() {
        match c0 {
            0x01..=0x7F => ret.push((c0 as u8) as char),
            0xC0..=0xDF => { // c0 starts two-byte encoding
                let c1 = src_iter.next();
                let c1 = match c1 { // c1 is UTF-8 continuation byte
                    Some(0x80..=0xBF) => c1.unwrap(),
                    Some(c1) => return Err(
                        ClassParseError::InvalidByteSequence(
                            String::from(bad_bytes_msg),
                            vec![c0 as u8, c1 as u8])),
                    None => return Err(
                        ClassParseError::from(
                            sized_io::ReadError::NotEnoughBytes(2))),
                };
                ret.push(
                    std::char::from_u32(((c0 & 0x1F) << 6) + (c1 & 0x3F))
                        .unwrap());
            },
            0xE0..=0xEF => { // c0 starts three-byte encoding
                let c1 = src_iter.next();
                let c1 = match c1 { // c1 is a UTF-8 continuation byte
                    Some(0x80..=0xBF) => c1.unwrap(),
                    Some(c1) => return Err(
                        ClassParseError::InvalidByteSequence(
                            String::from(bad_bytes_msg),
                            vec![c0 as u8, c1 as u8])),
                    None => return Err(
                        ClassParseError::from(
                            sized_io::ReadError::NotEnoughBytes(2))),
                };
                let c2 = src_iter.next();
                let c2 = match c2 { // c2 is a UTF-8 continuation byte
                    Some(0x80..=0xBF) => c2.unwrap(),
                    Some(c2) => return Err(
                        ClassParseError::InvalidByteSequence(
                            String::from(bad_bytes_msg),
                            vec![c0 as u8, c1 as u8, c2 as u8])),
                    None => return Err(
                        ClassParseError::from(
                            sized_io::ReadError::NotEnoughBytes(3))),
                };
                let b = ((c0 & 0xF) << 12) + ((c1 & 0x3F) << 6) + (c2 & 0x3F);
                match b {
                    // code_point(c0, c1, c2) is primary surrogate
                    0xD800..=0xDBFF => {
                        let c3 = src_iter.next();
                        let c3 = match c3 {
                            Some(0xED) => c3.unwrap(),
                            Some(c3) => return Err(
                                ClassParseError::InvalidByteSequence(
                                    String::from(
                                        "Found UTF-16 primary surrogate not followed by UTF-16 secondary surrogate"),
                                    vec![c0 as u8, c1 as u8, c2 as u8, c3 as u8])),
                            None => return Err(
                                ClassParseError::from(
                                    sized_io::ReadError::NotEnoughBytes(6))),
                        };
                        let c4 = src_iter.next();
                        let c4 = match c4 {
                            // c4 is secondary surrogate encoding second byte
                            Some(0xB0..=0xBF) => c4.unwrap(),
                            Some(c4) => return Err(
                                ClassParseError::InvalidByteSequence(
                                    String::from(
                                        "Found UTF-16 primary surrogate not followed by UTF-16 secondary surrogate"),
                                    vec![c0 as u8, c1 as u8, c2 as u8, c3 as u8, c4 as u8])),
                            None => return Err(
                                ClassParseError::from(
                                    sized_io::ReadError::NotEnoughBytes(6))),
                        };
                        let c5 = src_iter.next();
                        let c5 = match c5 {
                            Some(0x80..=0xBF) => c5.unwrap(),
                            Some(c5) => return Err(
                                ClassParseError::InvalidByteSequence(
                                    String::from(
                                        "Found UTF-16 primary surrogate not followed by UTF-16 secondary surrogate"),
                                    vec![c0 as u8, c1 as u8, c2 as u8, c3 as u8, c4 as u8, c5 as u8])),
                            None => return Err(
                                ClassParseError::from(
                                    sized_io::ReadError::NotEnoughBytes(6))),
                        };
                        ret.push(std::char::from_u32(0x10000 + ((c1 & 0xF) << 16) + ((c2 & 0x3F) << 10) + ((c4 & 0xF) << 6) + (c5 & 0x3F)).unwrap());
                    },
                    // code_point(c0, c1, c2) is secondary surrogate
                    0xDC00..=0xDFFF => return Err(
                        ClassParseError::InvalidByteSequence(
                            String::from(
                                "Found unpaired UTF-16 secondary surrogate"),
                            vec![c0 as u8, c1 as u8, c2 as u8])),
                    // code_point(c0, c1, c2) is valid UTF-8 three-byte code
                    // point
                    _ => ret.push(std::char::from_u32(b).unwrap()),
                }
            },
            _ => panic!("Got UTF-8-encoded string instead of jvm8-encoded"),
        }
    }
    Ok(ret)
}

fn read_utf8_cp_entry(src: &mut Read) -> Result<CPEntry, ClassParseError> {
    let length = read_u16(src)?.into();
    Ok(CPEntry::Utf8(
            convert_jvm8_to_string(&(sized_io::read_bytes(src, length)?))?))
}

fn read_cp_entry(src: &mut Read) -> Result<CPEntry, ClassParseError> {
    match read_u8(src) {
        Ok(0x01) => Ok(read_utf8_cp_entry(src)?),
        Ok(0x03) => Ok(CPEntry::Integer(read_u32(src)?)),
        Ok(0x04) => Ok(CPEntry::Float(read_u32(src)?)),
        Ok(0x05) => Ok(CPEntry::Long(read_u32(src)?, read_u32(src)?)),
        Ok(0x06) => Ok(CPEntry::Double(read_u32(src)?, read_u32(src)?)),
        Ok(0x07) => Ok(CPEntry::Class(read_u16(src)?)),
        Ok(0x08) => Ok(CPEntry::String(read_u16(src)?)),
        Ok(0x09) => Ok(CPEntry::Fieldref(read_u16(src)?, read_u16(src)?)),
        Ok(0x0A) => Ok(CPEntry::Methodref(read_u16(src)?, read_u16(src)?)),
        Ok(0x0B) => Ok(
                CPEntry::InterfaceMethodref(read_u16(src)?, read_u16(src)?)),
        Ok(0x0C) => Ok(CPEntry::NameAndType(read_u16(src)?, read_u16(src)?)),
        Ok(0x0F) => Ok(CPEntry::MethodHandle(
                ReferenceKind::from_u8(read_u8(src)?)?,
                read_u16(src)?)),
        Ok(0x10) => Ok(CPEntry::MethodType(read_u16(src)?)),
        Ok(0x12) => Ok(CPEntry::InvokeDynamic(read_u16(src)?, read_u16(src)?)),
        Ok(n) => Err(ClassParseError::InvalidConstantPoolEntryTag(n)),
        Err(e) => Err(ClassParseError::from(e)),
    }
}

pub type ConstantPool = Vec<CPEntry>;

pub trait AccessFlagged {
    /// The class, field, or method is declared public: it may be accessed
    /// anywhere the declaring class or class file can be accessed.
    fn is_public(&self) -> bool; // 0x0001
    /**
     * The class, field, or method is declared private: it may not be accessed
     * from outside of the class where it is declared; it may not be a top-
     * level class.
     */
    fn is_private(&self) -> bool; // 0x0002
    /**
     * The class, field, or method is declared protected: it may not be
     * accessed by any class which is neither in the same package nor a
     * subclass of the declaring class; it may not be a top-level class.
     */
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
    /// The method is declared synchronized. A class or field cannot be
    /// synchronized.
    fn is_synchronized(&self) -> bool; // 0x0020
    /// The field is declared volatile: it must not be cached. A class or method
    /// cannot be volatile.
    fn is_volatile(&self) -> bool; // 0x0040
    /**
     * The method is a bridge method. A bridge method is generated when a
     * generic method is "overridden" by a method with a different erasure.
     * For example, given the following classes, the compiler erases
     * Node<T>.setData(T) to Node.setData(Object) and generates a method
     * MyNode.setData(Object) which calls MyNode.setData(Integer) with the
     * proper casts.
     *
     * public class Node<T> { // The compiler erases this into Node
     *     private T data; // The compiler erases this into Object data
     *
     *     public Node(T data) { // The compiler erases this into Node(Object)
     *         this.data = data;
     *     }
     *
     *     public void setData(T data) {
     *         // The compiler erases this into setData(Object)
     *         this.data = data;
     *     }
     * }
     *
     * public class MyNode extends Node<Integer> {
     *     public MyNode(Integer data) {
     *         super(data + 1);
     *     }
     *
     *     @Override
     *     public void setData(Integer data) {
     *         super.setData(data + 1);
     *     }
     * }
     *
     * MyNode.setData(Object) is generated as a delegate to
     * MyNode.setData(Integer) so that calling setData on a MyNode object with
     * a non-Integer argument will produce an exception instead of behaving
     * badly in silence.
     * A class or field cannot be a bridge.
     */
    fn is_bridge(&self) -> bool; // 0x0040
    /**
     * A transient field is not serialized by the default Java serialization
     * functions. A transient field should be re-initialized on deserialization
     * by either the no-args constructor (if it is not dependent on the values
     * of non-transient fields) or a custom deserializer with signature and
     * exceptions "private void readObject(ObjectInputStream) throws
     * IOException, ClassNotFoundException".
     */
    fn is_transient(&self) -> bool; // 0x0080
    /// The method has variable arity. A class or field cannot be varargs.
    fn is_varargs(&self) -> bool; // 0x0080
    /// The method was not implemented in Java. A class or field cannot be
    /// native.
    fn is_native(&self) -> bool; // 0x0100
    /// The class is an interface. A method or field cannot be an interface.
    fn is_interface(&self) -> bool; // 0x0200
    /**
     * The class or method is abstract. An abstract class cannot be instantiated
     * directly. An abstract method does not have a body and must be implemented
     * by any instantiable subclass of its parent class. A field cannot be
     * abstract.
     */
    fn is_abstract(&self) -> bool; // 0x0400
    /**
     * Require all floating point operations in the method or class to remain
     * adherent to IEEE 754 in all intermediate steps. Default Java behavior is
     * to allow extended exponent range.
     */
    fn is_strict(&self) -> bool; // 0x0800
    /// The class, method, or field is not present in the associated source.
    fn is_synthetic(&self) -> bool; // 0x1000
    /// The class is an annotation type: declared as "@interface".
    fn is_annotation(&self) -> bool; // 0x2000
    /// The class or field is an enum or enum constant.
    fn is_enum(&self) -> bool; // 0x4000
}

pub struct Attribute {
    attribute_name_index: u16,
    attribute_info: Vec<u8>,
}

pub struct JavaField {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

impl JavaField {
    pub const fn new(
            access_flags: u16,
            name_index: u16,
            descriptor_index: u16,
            attributes: Vec<Attribute>)
            -> JavaField {

        JavaField {
            access_flags,
            name_index,
            descriptor_index,
            attributes,
        }
    }

    pub fn name<'a>(&self, cp: &'a ConstantPool) -> &'a str {
        let entry = &cp[self.name_index as usize];
        if let &CPEntry::Utf8(ref ret) = entry {
            ret
        } else {
            panic!("Expected Utf8 entry, found {:?}", entry);
        }
    }

    pub fn r#type<'a>(&self, cp: &'a mut ConstantPool) -> &'a JavaType {
        let entry = &mut cp[self.descriptor_index as usize];
        match entry {
            &mut CPEntry::Utf8(ref s) => {
                match JavaType::from_str(s) {
                    Ok(ret) => {
                        *entry = CPEntry::Type(ret);
                        if let CPEntry::Type(ref r) = entry {
                            r
                        } else {
                            panic!("Someone has been monkeying around with the constant pool!");
                        }
                    }
                    Err(e) => panic!(
                            "Expected type descriptor string, found {}", s),
                }
            },
            &mut CPEntry::Type(ref ret) => ret,
            entry => panic!("Expected Utf8 or Type entry, found {:?}", entry),
        }
    }
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

pub struct JavaMethod {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

pub struct JavaClass {
    major_version: u16,
    minor_version: u16,
    constant_pool: Vec<CPEntry>,
    access_flags: u16,
    this_class: u16,
    super_class: u16,
    interfaces: Vec<u16>,
    fields: Vec<JavaField>,
    methods: Vec<JavaMethod>,
    attributes: Vec<Attribute>,
}

pub fn read_class(src: &mut Read) -> Result<JavaClass, ClassParseError> {
    match read_u32(src)? {
        0xCAFE_BABE => {},
        n => return Err(ClassParseError::InvalidMagicNumber(n)),
    }
    // TODO: implement
    Err(ClassParseError::NotImplemented(String::from("read_class(&mut Read)")))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typifies_byte() {
        assert_eq!(JavaType::from_str("B").unwrap(), JavaType::Byte);
    }

    #[test]
    fn typifies_char() {
        assert_eq!(JavaType::from_str("C").unwrap(), JavaType::Char);
    }

    #[test]
    fn typifies_double() {
        assert_eq!(JavaType::from_str("D").unwrap(), JavaType::Double);
    }

    #[test]
    fn typifies_float() {
        assert_eq!(JavaType::from_str("F").unwrap(), JavaType::Float);
    }

    #[test]
    fn typifies_int() {
        assert_eq!(JavaType::from_str("I").unwrap(), JavaType::Int);
    }

    #[test]
    fn typifies_long() {
        assert_eq!(JavaType::from_str("J").unwrap(), JavaType::Long);
    }
}
