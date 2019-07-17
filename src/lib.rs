#![feature(box_syntax)]

use nom::{
    self,
    combinator as comb,
    Err,
    IResult,
    Needed
};

use sized_io::{read_u8, read_u16, read_u32};

use std::io::{Error, ErrorKind, Read, Write};
use std::str::FromStr;

mod sized_io {
    use std::io::{self, Read, Write};

    pub fn read_u8(src: &mut dyn Read) -> io::Result<u8> {
        let mut buf = [0; 1];
        src.read_exact(&mut buf)?;
        Ok(buf[0])
    }
    
    pub fn read_u16(src: &mut dyn Read) -> io::Result<u16> {
        let mut buf = [0; 2];
        src.read_exact(&mut buf)?;
        Ok(((buf[0] as u16) << 8) + (buf[1] as u16))
    }
    
    pub fn read_u32(src: &mut dyn Read) -> io::Result<u32> {
        let mut buf = [0; 4];
        src.read_exact(&mut buf)?;
        Ok(((buf[0] as u32) << 24) + ((buf[1] as u32) << 16) + ((buf[2] as u32) << 8) + (buf[3] as u32))
    }

    pub fn read_bytes(src: &mut dyn Read, length: u64) -> io::Result<Vec<u8>> {
        let mut handle = src.take(length);
        let mut buf = vec![];
        if let Err(e) = handle.read_to_end(&mut buf) {
            Err(Error::new(
                e.kind(),
                format!(
                    "Expected {} bytes, but ran into an error instead: {}",
                    length, e)))
        } else {
            Ok(buf)
        }
    }

    pub fn write_u8(out: &mut dyn Write, val: u8) -> io::Result<()> {
        let vals = [val];
        out.write_all(&vals[..])
    }

    pub fn write_u16(out: &mut dyn Write, val: u16) -> io::Result<()> {
        let vals = [(val >> 8) as u8, val as u8];
        out.write_all(&vals[..])
    }

    pub fn write_u32(out: &mut dyn Write, val: u32) -> io::Result<()> {
        let vals = [(val >> 24) as u8, (val >> 16) as u8, (val >> 8) as u8, val as u8];
        out.write_all(&vals[..])
    }

    pub fn write_bytes(out: &mut dyn Write, vals: &[u8]) -> io::Result<()> {
        out.write_all(vals)
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
    /// A wrapper around an io::Error.
    IoError(Error),
    /// A wrapper
    NomError(Err<String>),
}

impl From<Error> for ClassParseError {
    fn from(base: Error) -> ClassParseError {
        ClassParseError::IoError(base)
    }
}

impl<E: std::fmt::Debug> From<Err<E>> for ClassParseError {
    fn from(base: Err<E>) -> ClassParseError {
        ClassParseError::NomError(base.convert())
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
    Void,
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

        // TODO: consider reimplementing with package nom
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
            b'V' => if is_func {
                Ok((JavaType::Void, 1))
            } else {
                Err(ClassParseError::InvalidTypeDesc(
                        TypeDescProblem::ExpectedType(b'V'), None))
            }
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
    /**
     * arg is 1-based index of a Utf8 entry in the constant pool which contains
     * the fully-qualified name of a class. If the class is an array class,
     * such as int[] or java.lang.Thread[], the encoding is [I or
     * [Ljava/lang/thread;. For any other fully qualified class name
     * foo.bar.Baz, the encoding is Lfoo/bar/Baz;.
     */
    Class(u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the contents of the string.
    String(u16),
    /**
     * first arg is 1-based index of the Class entry for the class that owns
     * the referenced field, second arg is 1-based index of the NameAndType
     * entry of the referenced field.
     */
    Fieldref(u16, u16),
    /// Same as Fieldref except that first arg must not be index of an
    /// interface and second arg must be a method.
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
    /**
     * first arg is reference kind, second arg is 1-based index of the
     * {Field,Method,InterfaceMethod}ref in the constant pool to interact
     * with.
     */
    MethodHandle(ReferenceKind, u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the method descriptor.
    MethodType(u16),
    /**
     * first arg is 0-based index into bootstrap methods array in the
     * attributes table, second arg is 1-based index of a NameAndType entry
     * representing the referenced method.
     */
    InvokeDynamic(u16, u16),
}

fn is_jvm8_single_start(c: u8) -> bool {
    c & 0x80 == 0 && c != 0
}

fn is_jvm8_continuation_byte(c: u8) -> bool {
    c & 0xC0 == 0x80
}

fn is_jvm8_double_start(c: u8) -> bool {
    c & 0xE0 == 0xC0
}

fn is_jvm8_triple_start(c: u8) -> bool {
    c & 0xF0 == 0xE0
}

fn is_jvm8_surrogate_start(c: u8) -> bool {
    c == 0xED
}

fn is_jvm8_lead_surr_second(c: u8) -> bool {
    c & 0xF0 == 0xA0
}

fn is_jvm8_trail_surr_second(c: u8) -> bool {
    c & 0xF0 == 0xB0
}

fn is_jvm8_single_byte(cs: &[u8]) -> bool {
    cs.len() == 1 && is_jvm8_single_start(cs[0])
}

fn is_jvm8_double_byte(cs: &[u8]) -> bool {
    cs.len() == 2 && is_jvm8_double_start(cs[0]) && is_jvm8_continuation_byte(cs[1])
}

fn is_jvm8_triple_byte(cs: &[u8]) -> bool {
    cs.len() == 3 && is_jvm8_triple_start(cs[0]) && is_jvm8_continuation_byte(cs[1]) && is_jvm8_continuation_byte(cs[2]) && (!is_jvm8_surrogate_start(cs[0]) || !is_jvm8_lead_surr_second(cs[1]) && !is_jvm8_trail_surr_second(cs[1]))
}

fn is_jvm8_sextuple_byte(cs: &[u8]) -> bool {
    cs.len() == 6 && is_jvm8_surrogate_start(cs[0]) && is_jvm8_lead_surr_second(cs[1]) && is_jvm8_continuation_byte(cs[2]) && is_jvm8_surrogate_start(cs[3]) && is_jvm8_trail_surr_second(cs[4]) && is_jvm8_continuation_byte(cs[5])
}

fn one_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::verify(nom::bytes::complete::take(1), is_jvm8_single_byte)
            .map(|(rem, bytes)| (rem, char::from(bytes[0])))
}

fn two_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::verify(nom::bytes::complete::take(2), is_jvm8_double_byte)
            .map(|(rem, bytes)| {
                let high_bits = bytes[0] as u32 & 0x1F;
                let low_bits = bytes[1] as u32 & 0x3F;
                (rem, std::char::from_u32((high_bits << 6) | low_bits))
            })
}

fn three_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::verify(nom::bytes::complete::take(3), is_jvm8_triple_byte)
            .map(|(rem, cs)| {
                let high_bits = bytes[0] as u32 & 0xF;
                let mid_bits = bytes[1] as u32 & 0x3F;
                let low_bits = bytes[2] as u32 & 0x3F;
                let code_point = (high_bits << 12) | (mid_bits << 6) | low_bits;
                (rem, std::char::from_u32(code_point))
            })
}

fn six_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::verify(nom::bytes::complete::take(6), is_jvm8_sextuple_byte)
            .map(|(rem, bytes)| {
                let high_high_bits = (bytes[1] as u32 & 0xF) + 0x1_0000;
                let low_high_bits = bytes[2] as u32 & 0x3F;
                let high_low_bits = bytes[4] as u32 & 0xF;
                let low_low_bits = bytes[5] as u32 & 0x3F;
                let high_bits = (high_high_bits << 6) | low_high_bits;
                let low_bits = (high_low_bits << 6) | low_low_bits;
                Ok((rem, (high_bits << 10) | low_bits))
            })
}

fn convert_jvm8_to_string_nom(bytes: &[u8]) -> IResult<&[u8], String> {
    alt!(empty | do_parse!(
        code_point: alt!(one_byte_point | two_byte_point | three_byte_point
                | six_byte_point) >>
        rest: convert_jvm8_to_string_nom >>
        (format!("{}{}", code_point, rest))
    ))
}

fn convert_jvm8_to_string(bytes: &[u8]) -> Result<String, ClassParseError> {
    let (rem, s) = convert_jvm8_to_string_nom(bytes)?;
    match rem {
        &[] => Ok(s),
        ref bytes => panic!(format!("`convert_jvm8_to_string_nom` should only return when it encounters an error or runs out of input. Returned Ok with {:?} bytes remaining: {:?}", bytes.len(), bytes)),
    }
}

fn read_utf8_cp_entry(src: &mut dyn Read) -> Result<CPEntry, ClassParseError> {
    let length = read_u16(src)?.into();
    Ok(CPEntry::Utf8(
            convert_jvm8_to_string(&(sized_io::read_bytes(src, length)?))?))
}

fn read_cp_entry(src: &mut dyn Read) -> Result<CPEntry, ClassParseError> {
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

type ConstantPool = Vec<CPEntry>;

fn read_constant_pool(len: u16, src: &mut dyn Read)
        -> Result<ConstantPool, ClassParseError> {

    let mut ret = vec![];
    for _ in 0..len {
        ret.push(read_cp_entry(src)?);
    }
    Ok(ret)
}

fn get_class_name(cp: &ConstantPool, idx: u16) -> String {
    let type_entry = match &cp[idx as usize - 1] {
        &CPEntry::Class(idx) => &cp[idx as usize - 1],
        entry => panic!("Expected CPEntry::Class, found {:?}", entry),
    };
    match type_entry {
        &CPEntry::Type(JavaType::Class(ref s)) => s.to_string(),
        &CPEntry::Utf8(ref s) => String::from(&s[1..s.len()-1]),
        entry => panic!("Expected CPEntry::Type or CPEntry::Utf8, found {:?}", entry),
    }
}

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
    /**
     * The JVM should ignore the class name in all uses of invokespecial in
     * this class file. Beginning with 7u13, Oracle's JVM treated all class
     * files as if this attribute were present, regardless of whether it
     * actually is.
     */
    fn is_super_special(&self) -> bool; // 0x0020
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
     * ```public class Node<T> { // The compiler erases this into Node
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
     * }```
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

fn read_attributes(num_attributes: u16, src: &mut dyn Read)
        -> Result<Vec<Attribute>, ClassParseError> {

    // TODO: implement
    Ok(vec![])
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

fn read_fields(num_fields: u16, src: &mut dyn Read)
        -> Result<Vec<JavaField>, ClassParseError> {

    // TODO: implement
    Ok(vec![])
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

pub struct JavaMethod {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<Attribute>,
}

impl JavaMethod {
    /// Get the name of this method.
    pub fn name(&self, constant_pool: &ConstantPool) -> String {
        match &constant_pool[self.name_index as usize + 1] {
            &CPEntry::Utf8(ref s) => s.to_string(),
            // TODO: change to log and return ""?
            ref entry => panic!("Expected string, found {:?}", &entry),
        }
    }

    /**
     * Get the type of this method. If the relevant item in the constant pool
     * is a type descriptor, convert it to a type object and return that type
     * object. If it's already a type object, return that type object.
     */
    pub fn r#type(&self, constant_pool: &ConstantPool) -> JavaType {
        match &constant_pool[self.descriptor_index as usize - 1] {
            &CPEntry::Utf8(ref s) => match JavaType::from_str(s) {
                Ok(t) => t,
                Err(e) => panic!("{:?}", e),
            },
            &CPEntry::Type(ref t) => t.clone(),
            // TODO: convert to log and return ""?
            ref entry => panic!(
                    "Expected String or JavaType, found {:?}", &entry),
        }
    }

    /**
     * Like `JavaMethod::r#type` except that the newly created type object
     * replaces the string it was produced from if a conversion is made.
     */
    pub fn convert_type(&self, constant_pool: &mut ConstantPool) -> JavaType {
        let r#type = self.r#type(constant_pool);
        let entry = &mut constant_pool[self.descriptor_index as usize - 1];
        match entry {
            &mut CPEntry::Utf8(_) => *entry = CPEntry::Type(r#type.clone()),
            &mut CPEntry::Type(_) => {},
            _ => panic!("Expected String or JavaType, found {:?}", &entry),
        }
        r#type
    }
}

fn read_methods(num_methods: u16, src: &mut dyn Read)
        -> Result<Vec<JavaMethod>, ClassParseError> {

    // TODO: implement
    Ok(vec![])
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
    constant_pool: ConstantPool,
    access_flags: u16,
    this_class: u16,
    super_class: u16,
    interfaces: Vec<u16>,
    fields: Vec<JavaField>,
    methods: Vec<JavaMethod>,
    attributes: Vec<Attribute>,
}

fn read_interfaces(num_interfaces: u16, src: &mut dyn Read)
        -> Result<Vec<u16>, ClassParseError> {

    // TODO: implement
    Ok(vec![])
}

impl JavaClass {
    pub fn new(
            version: ClassFileVersion,
            constant_pool: ConstantPool,
            access_flags: u16,
            this_class: u16,
            super_class: u16,
            interfaces: Vec<u16>,
            fields: Vec<JavaField>,
            methods: Vec<JavaMethod>,
            attributes: Vec<Attribute>)
            -> JavaClass {

        JavaClass {
            version,
            constant_pool,
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
        //     methods: [JavaMethod; num_methods],
        //     num_attributes: u16,
        //     attributes: [Attribute; num_attributes],
        // }
        match read_u32(src)? {
            0xCAFE_BABE => {},
            n => return Err(ClassParseError::InvalidMagicNumber(n)),
        }
        let minor_version = read_u16(src)?;
        let major_version = read_u16(src)?;
        let version = ClassFileVersion::new(major_version, minor_version);
        let cp_size = read_u16(src)?;
        let constant_pool = read_constant_pool(cp_size, src)?;
        let access_flags = read_u16(src)?;
        let this_class = read_u16(src)?;
        let super_class = read_u16(src)?;
        let num_interfaces = read_u16(src)?;
        let interfaces = read_interfaces(num_interfaces, src)?;
        let num_fields = read_u16(src)?;
        let fields = read_fields(num_fields, src)?;
        let num_methods = read_u16(src)?;
        let methods = read_methods(num_methods, src)?;
        let num_attributes = read_u16(src)?;
        let attributes = read_attributes(num_attributes, src)?;
        Ok(JavaClass::new(
                version,
                constant_pool,
                access_flags,
                this_class,
                super_class,
                interfaces,
                fields,
                methods,
                attributes))
    }

    pub fn write(&self, &mut dyn Write) -> io::Result<()> {
        // TODO: implement
        Ok(())
    }

    pub fn get_class_file_version(&self) -> ClassFileVersion {
        self.version
    }

    pub fn get_name(&self) -> String {
        get_class_name(&self.constant_pool, self.this_class)
    }

    pub fn get_superclass_name(&self) -> String {
        get_class_name(&self.constant_pool, self.super_class)
    }

    pub fn get_interface_names(&self) -> Vec<String> {
        self.interfaces.iter()
                .map(|&idx| get_class_name(&self.constant_pool, idx))
                .collect()
    }

    pub fn get_fields(&self) -> &Vec<JavaField> {
        &self.fields
    }

    pub fn get_methods(&self) -> &Vec<JavaMethod> {
        &self.methods
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

    #[test]
    fn typifies_float() {
        assert_eq!(JavaType::from_str("F").unwrap(), JavaType::Float);
    }

    #[test]
    fn typifies_double() {
        assert_eq!(JavaType::from_str("D").unwrap(), JavaType::Double);
    }

    #[test]
    fn typifies_double_array() {
        assert_eq!(
                JavaType::from_str("[D").unwrap(),
                JavaType::Array(box JavaType::Double));
    }

    #[test]
    fn typifies_class_name() {
        assert_eq!(
                JavaType::from_str("Ljava/lang/String;").unwrap(),
                JavaType::Class(String::from("java/lang/String")));
    }

    #[test]
    fn typifies_void_to_void() {
        assert_eq!(
                JavaType::from_str("()V").unwrap(),
                JavaType::Method(box vec![], box JavaType::Void));
    }
}
