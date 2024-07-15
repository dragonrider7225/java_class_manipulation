use extended_io as eio;

use nom::Err;

use num_enum::{IntoPrimitive, TryFromPrimitive};

use std::{
    convert::{TryFrom, TryInto},
    error::Error,
    fmt::{self, Debug, Display, Formatter},
    hint::unreachable_unchecked,
    io::{self, Read, Write},
    ops::Index,
};

use crate::{
    parsers::NomFlatError,
    types::{field::JavaFieldType, JavaType, QualifiedClassName},
    ClassParseError, CrateResult, FieldRef, MethodRef,
};

use super::JavaIdentifier;

pub(crate) type CPAccessResult<T> = Result<T, CPAccessError>;

/// The kind of action a `CPEntry::MethodHandle` represents.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, IntoPrimitive, PartialEq, TryFromPrimitive)]
pub enum ReferenceKind {
    /// Read an instance-specific (non-static) field of an object.
    ReadInstanceField = 1,
    /// Read a static field of a class.
    ReadStaticField = 2,
    /// Write an instance-specific (non-static) field of an object.
    WriteInstanceField = 3,
    /// Write a static field of a class.
    WriteStaticField = 4,
    /// Invoke an instance method by walking up the class hierarchy of the
    /// object the method is invoked on. `InvokeVirtual` differs from
    /// `InvokeSpecial` in that `InvokeVirtual` begins from the runtime class
    /// of the object reference whereas `InvokeSpecial` begins from the class
    /// specified in the associated `CPEntry::Methodref`.
    InvokeVirtual = 5,
    /// Invoke a static method of a class.
    InvokeStatic = 6,
    /// Invoke an instance method by walking up the class hierarchy of the
    /// class specified in the associated `CPEntry::Methodref`. The runtime
    /// class of the object reference must be a subclass of the class at which
    /// the walking of the class hierarchy begins.
    InvokeSpecial = 7,
    /// Invoke a method named `<init>`. The class specified in the associated
    /// `CPEntry::Methodref` must not be an interface.
    InvokeNew = 8,
    /// Invoke a method from an interface. The class specified in the
    /// associated `CPEntry::Methodref` must be an interface which is
    /// implemented (directly or indirectly) by the runtime class of the
    /// object reference.
    InvokeInterface = 9,
}

fn read_utf8_cp_entry(src: &mut dyn Read) -> CrateResult<CPEntry> {
    let length = eio::read_u16(src)?.into();
    let bytes = eio::read_bytes(src, length)?;
    Ok(CPEntry::Utf8(crate::convert_jvm8_to_string(&bytes)?))
}

/// A constant pool was accessed in an improper way.
#[derive(Debug)]
pub enum CPAccessError {
    /// Attempted to access invalid index in the constant pool
    IndexOutOfBounds {
        /// The invalid index.
        request: u16,
        /// The number of elements in the constant pool. Also the maximum valid index in the
        /// constant pool if non-zero.
        size: u16,
    },
    /// Attempted to access constant pool entry of incorrect type
    BadEntryType {
        /// The index with an unexpected entry type.
        request: u16,
        /// The expected entry type.
        expected: CPEntryType,
        /// The unexpected entry type.
        actual: CPEntryType,
    },
    /// Failed to convert constant pool entry into a Rust type
    FailedTypeConversion {
        /// The index with an invalid entry type.
        request: u16,
        /// The invalid entry type.
        source: CPEntryType,
        /// The conversion error if available.
        cause: Option<Box<dyn Error + Send + Sync>>,
    },
    /// Failed to parse a string into a value.
    FailedParse(NomFlatError),
    /// Attempted to add a new entry to a full constant pool.
    ConstantPoolFull,
    /// An attempt to parse a value from the constant pool failed because the element of the
    /// constant pool at index `request` references the element at another index which couldn't be
    /// parsed into a valid value.
    BadTarget {
        /// The index of the entry which contains a reference to another entry in the constant pool
        /// which couldn't be parsed.
        request: u16,
        /// The error which was produced from reentry of the constant pool.
        cause: Box<CPAccessError>,
    },
}

impl Clone for CPAccessError {
    fn clone(&self) -> Self {
        match self {
            CPAccessError::IndexOutOfBounds { request, size } => CPAccessError::IndexOutOfBounds {
                request: *request,
                size: *size,
            },
            CPAccessError::BadEntryType {
                request,
                expected,
                actual,
            } => CPAccessError::BadEntryType {
                request: *request,
                expected: *expected,
                actual: *actual,
            },
            CPAccessError::FailedTypeConversion {
                request, source, ..
            } => CPAccessError::FailedTypeConversion {
                request: *request,
                source: *source,
                cause: None,
            },
            CPAccessError::FailedParse(e) => CPAccessError::FailedParse(e.clone()),
            CPAccessError::ConstantPoolFull => CPAccessError::ConstantPoolFull,
            CPAccessError::BadTarget { request, cause } => CPAccessError::BadTarget {
                request: *request,
                cause: cause.clone(),
            },
        }
    }
}

impl Display for CPAccessError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CPAccessError::IndexOutOfBounds { request, size } => write!(
                f,
                "Tried to access index {} in constant pool of size {}",
                request, size,
            ),
            CPAccessError::BadEntryType {
                request,
                expected,
                actual,
            } => write!(
                f,
                "Tried to get constant pool entry of type {} at index {} when actual type is {}",
                expected, request, actual,
            ),
            CPAccessError::FailedTypeConversion {
                request,
                source,
                cause,
            } => write!(
                f,
                "Tried to convert entry of type {} at index {} into another type but got error {:?}",
                source, request, cause,
            ),
            CPAccessError::ConstantPoolFull => write!(f, "Ran out of space in constant pool"),
            CPAccessError::FailedParse(e) => write!(f, "Parse failure: {}", e),
            CPAccessError::BadTarget { request, cause } => write!(
                f,
                r#"Tried to get value from constant pool entry at {} which references another element of the constant pool but got error "{}" when attempting to get a value from that element."#,
                request, cause,
            ),
        }
    }
}

impl Error for CPAccessError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            CPAccessError::FailedTypeConversion { cause, .. } => {
                cause.as_ref().map(|b| &**b as &(dyn Error + 'static))
            }
            _ => None,
        }
    }
}

impl From<NomFlatError> for CPAccessError {
    fn from(base: NomFlatError) -> CPAccessError {
        CPAccessError::FailedParse(base)
    }
}

impl<E: Debug> From<Err<E>> for CPAccessError {
    fn from(base: Err<E>) -> Self {
        <Self as From<NomFlatError>>::from(NomFlatError::from(base))
    }
}

/// An entry in the constant pool.
#[derive(Debug)]
pub enum CPEntry {
    /// A string encoded in a form of UTF-8 modified according to the following rules:
    /// * All code points less than 0x0001_0000 except 0 are represented as in regular UTF-8.
    /// * 0 is represented by 0b11000000 0b10000000 -- the two-byte encoding for 0 -- to avoid
    ///   strings which contain NUL being incorrectly determined to end at that NUL.
    /// * All other code points are encoded in UTF-16 and then the surrogates which make up the
    ///   encoding are themselves encoded in UTF-8. The result of this two-step encoding process is
    ///   that the codepoint 0bABCDE_FGHIJKLM_NOPQRSTU is represented by 0b11101101 0b1010VWXY
    ///   0b10FGHIJK 0b11101101 0b1011LMNO 0b10PQRSTU, where 0bVWXY is 0bABCDE - 1. 0bABCDE - 1 is
    ///   guaranteed to fit into four bits because the maximum codepoint has been defined to be
    ///   0x10_FFFF so bit A is only set when bits B, C, D, and E are unset.
    Utf8(String),
    /// An `int` value.
    Integer(i32),
    /// A `float` value.
    Float(f32),
    /// A `long` value.
    Long(i64),
    /// A `double` value.
    Double(f64),
    /// The 1-based index of a Utf8 entry in the constant pool which contains the fully-qualified
    /// name of a class. If the class is an array class, such as int[] or java.lang.Thread[], the
    /// encoding is [I or [Ljava/lang/thread;. For any other fully-qualified class name foo.bar.Baz,
    /// the encoding is Lfoo/bar/Baz;.
    Class(u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains the contents of
    /// the string.
    String(u16),
    /// first arg is 1-based index of the Class entry for the class that owns the referenced field,
    /// second arg is 1-based index of the NameAndType entry of the referenced field.
    Fieldref(u16, u16),
    /// Same as Fieldref except that first arg must not be index of an interface and second arg must
    /// be a method.
    Methodref(u16, u16),
    /// Same as Methodref except that first arg must be index of an interface.
    InterfaceMethodref(u16, u16),
    /// first arg is 1-based index of a Utf8 entry in the constant pool which contains the name of
    /// the reference, second arg is 1-based index of a Utf8 entry in the constant pool which
    /// contains the type descriptor of the reference.
    NameAndType(u16, u16),
    /// first arg is reference kind, second arg is 1-based index of the
    /// {Field,Method,InterfaceMethod}ref in the constant pool to interact with.
    MethodHandle(ReferenceKind, u16),
    /// arg is 1-based index of a Utf8 entry in the constant pool which contains the method
    /// descriptor.
    MethodType(u16),
    /// first arg is 0-based index into bootstrap methods array in the attributes table, second arg
    /// is 1-based index of a NameAndType entry representing the referenced method.
    InvokeDynamic(u16, u16),
    /// An empty constant pool entry representing the hole in the constant pool following the 8-byte
    /// constants Long and Double.
    After8Byte,
}

impl CPEntry {
    /// Get the entry type of `self`.
    pub fn r#type(&self) -> CPEntryType {
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

    /// Get the tag used for `self` in a class file.
    pub fn tag(&self) -> u8 {
        match self.r#type() {
            CPEntryType::After8Byte => panic!("No tag for constant pool hole"),
            entry_type => entry_type.into(),
        }
    }

    /// Read a constant pool entry from `src`.
    pub fn read(src: &mut dyn Read) -> CrateResult<CPEntry> {
        match CPEntryType::try_from(eio::read_u8(src)?) {
            Ok(CPEntryType::Utf8) => Ok(read_utf8_cp_entry(src)?),
            Ok(CPEntryType::Integer) => Ok(CPEntry::Integer(eio::read_i32(src)?)),
            Ok(CPEntryType::Float) => Ok(CPEntry::Float(eio::read_f32(src)?)),
            Ok(CPEntryType::Long) => Ok(CPEntry::Long(eio::read_i64(src)?)),
            Ok(CPEntryType::Double) => Ok(CPEntry::Double(eio::read_f64(src)?)),
            Ok(CPEntryType::Class) => Ok(CPEntry::Class(eio::read_u16(src)?)),
            Ok(CPEntryType::String) => Ok(CPEntry::String(eio::read_u16(src)?)),
            Ok(CPEntryType::Fieldref) => {
                let owner_idx = eio::read_u16(src)?;
                let name_and_type_idx = eio::read_u16(src)?;
                Ok(CPEntry::Fieldref(owner_idx, name_and_type_idx))
            }
            Ok(CPEntryType::Methodref) => {
                let owner_idx = eio::read_u16(src)?;
                let name_and_type_idx = eio::read_u16(src)?;
                Ok(CPEntry::Methodref(owner_idx, name_and_type_idx))
            }
            Ok(CPEntryType::InterfaceMethodref) => {
                let owner_idx = eio::read_u16(src)?;
                let name_and_type_idx = eio::read_u16(src)?;
                Ok(CPEntry::InterfaceMethodref(owner_idx, name_and_type_idx))
            }
            Ok(CPEntryType::NameAndType) => {
                let name_idx = eio::read_u16(src)?;
                let type_idx = eio::read_u16(src)?;
                Ok(CPEntry::NameAndType(name_idx, type_idx))
            }
            Ok(CPEntryType::MethodHandle) => {
                let rk = ReferenceKind::try_from(eio::read_u8(src)?)?;
                let ref_idx = eio::read_u16(src)?;
                Ok(CPEntry::MethodHandle(rk, ref_idx))
            }
            Ok(CPEntryType::MethodType) => Ok(CPEntry::MethodType(eio::read_u16(src)?)),
            Ok(CPEntryType::InvokeDynamic) => {
                let bootstrap_method_idx = eio::read_u16(src)?;
                let name_and_type_idx = eio::read_u16(src)?;
                Ok(CPEntry::InvokeDynamic(
                    bootstrap_method_idx,
                    name_and_type_idx,
                ))
            }
            Ok(CPEntryType::After8Byte) => {
                Err(ClassParseError::InvalidConstantPoolEntryTag { actual: 0 })?
            }
            Err(e) => Err(ClassParseError::InvalidConstantPoolEntryTag { actual: e.number })?,
        }
    }

    /// Write `self` to `sink`.
    pub fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        if let CPEntry::After8Byte = self {
            return Ok(());
        }
        eio::write_u8(sink, self.tag())?;
        match self {
            CPEntry::Utf8(s) => crate::write_jvm8(sink, s)?,
            CPEntry::Integer(n) => eio::write_i32(sink, *n)?,
            CPEntry::Float(f) => eio::write_f32(sink, *f)?,
            CPEntry::Long(n) => eio::write_i64(sink, *n)?,
            CPEntry::Double(f) => eio::write_f64(sink, *f)?,
            CPEntry::Class(idx) => eio::write_u16(sink, *idx)?,
            CPEntry::String(idx) => eio::write_u16(sink, *idx)?,
            CPEntry::Fieldref(idx1, idx2)
            | CPEntry::Methodref(idx1, idx2)
            | CPEntry::InterfaceMethodref(idx1, idx2)
            | CPEntry::NameAndType(idx1, idx2)
            | CPEntry::InvokeDynamic(idx1, idx2) => {
                eio::write_u16(sink, *idx1)?;
                eio::write_u16(sink, *idx2)?;
            }
            CPEntry::MethodHandle(rk, idx) => {
                eio::write_u8(sink, (*rk).into())?;
                eio::write_u16(sink, *idx)?;
            }
            CPEntry::MethodType(idx) => eio::write_u16(sink, *idx)?,
            // This branch is unreachable because the function returned before writing the tag if
            // `self` is `After8Byte`.
            CPEntry::After8Byte => unsafe { unreachable_unchecked() },
        }
        Ok(())
    }

    /// A "DebugWithContext" fake impl.
    pub fn to_string(&self, pool: &ConstantPool) -> CPAccessResult<String> {
        let ret = match self {
            CPEntry::Utf8(s) => format!(r#"Utf8("{}")"#, s),
            CPEntry::Integer(i) => format!("Integer({})", i),
            CPEntry::Float(f) => format!("Float({})", f),
            CPEntry::Long(l) => format!("Long({})", l),
            CPEntry::Double(d) => format!("Double({})", d),
            CPEntry::Class(idx) => format!("Class({}: {})", idx, pool.get(*idx)?.to_string(pool)?),
            CPEntry::String(idx) => {
                format!("String({}: {})", idx, pool.get(*idx)?.to_string(pool)?)
            }
            CPEntry::Fieldref(owner, nat) => format!(
                "Fieldref({}: {}, {}: {})",
                owner,
                pool.get(*owner)?.to_string(pool)?,
                nat,
                pool.get(*nat)?.to_string(pool)?
            ),
            CPEntry::Methodref(owner, nat) => format!(
                "Methodref({}: {}, {}: {})",
                owner,
                pool.get(*owner)?.to_string(pool)?,
                nat,
                pool.get(*nat)?.to_string(pool)?
            ),
            CPEntry::InterfaceMethodref(owner, nat) => format!(
                "InterfaceMethodref({}: {}, {}: {})",
                owner,
                pool.get(*owner)?.to_string(pool)?,
                nat,
                pool.get(*nat)?.to_string(pool)?
            ),
            CPEntry::NameAndType(name_idx, type_idx) => format!(
                "NameAndType({}: {}, {}: {})",
                name_idx,
                pool.get(*name_idx)?.to_string(pool)?,
                type_idx,
                pool.get(*type_idx)?.to_string(pool)?
            ),
            CPEntry::MethodHandle(rk, ref_idx) => format!(
                "MethodHandle({:?}, {}: {})",
                rk,
                ref_idx,
                pool.get(*ref_idx)?.to_string(pool)?
            ),
            CPEntry::MethodType(type_idx) => format!(
                "MethodType({}: {})",
                type_idx,
                pool.get(*type_idx)?.to_string(pool)?
            ),
            CPEntry::InvokeDynamic(bootstrap_idx, nat_idx) => format!(
                "InvokeDynamic({}: {}, {}: {})",
                bootstrap_idx,
                pool.get(*bootstrap_idx)?.to_string(pool)?,
                nat_idx,
                pool.get(*nat_idx)?.to_string(pool)?
            ),
            CPEntry::After8Byte => "After8Byte".to_string(),
        };
        Ok(ret)
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
            | (
                CPEntry::InterfaceMethodref(owner1, nat1),
                CPEntry::InterfaceMethodref(owner2, nat2),
            ) => owner1 == owner2 && nat1 == nat2,
            (CPEntry::NameAndType(name1, type1), CPEntry::NameAndType(name2, type2)) => {
                name1 == name2 && type1 == type2
            }
            (CPEntry::MethodHandle(kind1, idx1), CPEntry::MethodHandle(kind2, idx2)) => {
                kind1 == kind2 && idx1 == idx2
            }
            (CPEntry::MethodType(idx1), CPEntry::MethodType(idx2)) => idx1 == idx2,
            (
                CPEntry::InvokeDynamic(bootstrap1, nat1),
                CPEntry::InvokeDynamic(bootstrap2, nat2),
            ) => bootstrap1 == bootstrap2 && nat1 == nat2,
            (_, _) => false,
        }
    }
}

impl Eq for CPEntry {}

/// The tag on a constant pool entry.
#[repr(u8)]
#[derive(Clone, Copy, Debug, IntoPrimitive, TryFromPrimitive)]
pub enum CPEntryType {
    /// The entry is a string of Unicode codepoints encoded according to the modified version of
    /// UTF-8 described at
    /// <https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.7>.
    Utf8 = 0x01,
    /// The entry is a 4-byte integer value.
    Integer = 0x03,
    /// The entry is a 4-byte floating point value.
    Float = 0x04,
    /// The entry is an 8-byte integer value.
    Long = 0x05,
    /// The entry is an 8-byte floating point value.
    Double = 0x06,
    /// The entry is a reference to an entry of type Utf8 which represents the fully-qualified name
    /// of a class.
    Class = 0x07,
    /// The entry is a reference to an entry of type Utf8. An entry of type String represents a
    /// string literal in the original source file.
    String = 0x08,
    /// The entry is a pair of a reference to an entry of type Class and a reference to an entry of
    /// type NameAndType which refers to a field.
    Fieldref = 0x09,
    /// The entry is a pair of a reference to an entry of type Class which refers to a `class` or
    /// `enum` and a reference to an entry of type NameAndType which refers to a method.
    Methodref = 0x0A,
    /// The entry is a pair of a reference to an entry of type Class which refers to an `interface`
    /// and a reference to an entry of type NameAndType which refers to a method.
    InterfaceMethodref = 0x0B,
    /// The entry is a pair of a reference to an entry of type Utf8 which represents either
    /// `"<init>"` or a valid Java identifier and a reference to an entry of type Utf8 which
    /// represents the type of the identifier.
    NameAndType = 0x0C,
    /// The entry describes a way of interacting with a `Fieldref`, `Methodref`, or
    /// `InterfaceMethodref`. The entry is a pair of a number in the range `1..=9` and a reference
    /// to a *ref which must be a `Fieldref` if the number is in the range `1..=4`, a `Methodref` if
    /// the number is in the range `5..=8`, and an `InterfaceMethodref` if the number is `9`.
    MethodHandle = 0x0F,
    /// The entry is a reference to an entry of type `Utf8` which represents a method type.
    MethodType = 0x10,
    /// The entry is a pair of a reference to an element of the bootstrap method list of this class
    /// file and a reference to an entry of type `NameAndType` which represents a method name and
    /// type.
    InvokeDynamic = 0x12,
    /// The entry follows a `Long` or `Double`.
    After8Byte = 0x00,
}

impl Display for CPEntryType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

/// The constant pool of some Java class object. All indexing operations are 1-based.
#[derive(Debug, Default)]
pub struct ConstantPool {
    /// The actual constant pool.
    pool: Vec<CPEntry>,
}

impl ConstantPool {
    /// The maximum number of entries in the constant pool.
    const MAX_CAPACITY: usize = 0xFFFE;

    /// Create an empty constant pool.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new constant pool which will not resize until an entry is added while the pool
    /// contains `initial_capacity` entries.
    pub fn with_capacity(initial_capacity: usize) -> CPAccessResult<Self> {
        if initial_capacity > Self::MAX_CAPACITY {
            Err(CPAccessError::ConstantPoolFull)
        } else {
            Ok(Self {
                pool: Vec::with_capacity(initial_capacity),
            })
        }
    }

    /// Read a constant pool from `src`.
    pub fn read(src: &mut dyn Read) -> CrateResult<Self> {
        let len = eio::read_u16(src)?;
        let mut ret = Self::with_capacity(len as usize - 1)?;
        for _i in 1..len {
            let entry = CPEntry::read(src)?;
            if cfg!(debug_assertions) {
                assert_eq!(ret.add(entry)?, _i);
            } else {
                ret.pool.push(entry);
            }
        }
        Ok(ret)
    }

    /// Write self to `sink`.
    pub fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        eio::write_u16(sink, self.size() + 1)?;
        for entry in self.pool.iter() {
            entry.write(sink)?;
        }
        Ok(())
    }

    /// Get the number of entries in the constant pool.
    pub fn size(&self) -> u16 {
        let len = self.pool.len();
        if len > Self::MAX_CAPACITY {
            panic!(
                "Constant pool too large ({}). Maximum size is {}",
                len, 0xFFFE
            );
        }
        len.try_into().unwrap()
    }

    fn check_bounds(&self, idx: u16) -> CPAccessResult<()> {
        // idx only out of bounds (to the right) if *greater* than self.size() since the constant
        // pool is indexed from 1, not from 0.
        let size = self.size();
        if idx == 0 || size < idx {
            Err(CPAccessError::IndexOutOfBounds { request: idx, size })
        } else {
            Ok(())
        }
    }

    /// Get a reference to the entry in the constant pool at index `idx`.
    pub fn get(&self, idx: u16) -> CPAccessResult<&CPEntry> {
        self.check_bounds(idx)?;
        Ok(&self.pool[idx as usize - 1])
    }

    /// Get the boolean entry in the constant pool at index `idx`.
    pub fn get_bool(&self, idx: u16) -> CPAccessResult<bool> {
        Ok(self.get_int(idx)? != 0)
    }

    /// Get the 8-bit signed integer entry in the constant pool at index `idx`.
    pub fn get_byte(&self, idx: u16) -> CPAccessResult<i8> {
        let entry = self.get_int(idx)?;
        entry
            .try_into()
            .map_err(|_| CPAccessError::FailedTypeConversion {
                request: idx,
                source: CPEntryType::Integer,
                cause: None,
            })
    }

    /// Get the character entry in the constant pool at index `idx`.
    pub fn get_char(&self, idx: u16) -> CPAccessResult<char> {
        let value = self.get_int(idx)?;
        let bytes = value.to_be_bytes();
        let shorts = [
            u16::from_be_bytes([bytes[0], bytes[1]]),
            u16::from_be_bytes([bytes[2], bytes[3]]),
        ];
        let s = if bytes[..2] == [0, 0] {
            String::from_utf16_lossy(&shorts[1..])
        } else {
            String::from_utf16_lossy(&shorts)
        };
        s.chars().next().ok_or(CPAccessError::FailedTypeConversion {
            request: idx,
            source: CPEntryType::Integer,
            cause: None,
        })
    }

    /// Get the 16-bit signed integer entry in the constant pool at index `idx`.
    pub fn get_short(&self, idx: u16) -> CPAccessResult<i16> {
        let entry = self.get_int(idx)?;
        entry
            .try_into()
            .map_err(|_| CPAccessError::FailedTypeConversion {
                request: idx,
                source: CPEntryType::Integer,
                cause: None,
            })
    }

    /// Get the 32-bit signed integer entry in the constant pool at index `idx`.
    pub fn get_int(&self, idx: u16) -> CPAccessResult<i32> {
        match self.get(idx)? {
            CPEntry::Integer(i) => Ok(*i),
            e => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Integer,
                actual: e.r#type(),
            }),
        }
    }

    /// Get the 64-bit signed integer entry in the constant pool at index `idx`.
    pub fn get_long(&self, idx: u16) -> CPAccessResult<i64> {
        match self.get(idx)? {
            CPEntry::Long(l) => Ok(*l),
            e => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Long,
                actual: e.r#type(),
            }),
        }
    }

    /// Get the 32-bit floating-point entry in the constant pool at index `idx`.
    pub fn get_float(&self, idx: u16) -> CPAccessResult<f32> {
        match self.get(idx)? {
            CPEntry::Float(f) => Ok(*f),
            e => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Float,
                actual: e.r#type(),
            }),
        }
    }

    /// Get the 64-bit floating-point entry in the constant pool at index `idx`.
    pub fn get_double(&self, idx: u16) -> CPAccessResult<f64> {
        match self.get(idx)? {
            CPEntry::Double(d) => Ok(*d),
            e => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Double,
                actual: e.r#type(),
            }),
        }
    }

    /// Get a reference to the string at index `idx`.
    pub fn get_utf8(&self, idx: u16) -> CPAccessResult<&str> {
        match self.get(idx)? {
            CPEntry::Utf8(s) => Ok(s),
            entry => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Utf8,
                actual: entry.r#type(),
            }),
        }
    }

    /// Get the [`JavaFieldType`] at index `idx`.
    pub fn get_field_type(&self, idx: u16) -> CPAccessResult<JavaFieldType> {
        let entry = self.get_utf8(idx)?;
        entry.parse::<JavaFieldType>().map_err(CPAccessError::from)
    }

    /// Get a reference to the string literal at index `idx`.
    pub fn get_string(&self, idx: u16) -> CPAccessResult<&str> {
        match self.get(idx)? {
            &CPEntry::String(idx) => self.get_utf8(idx).map_err(|e| CPAccessError::BadTarget {
                request: idx,
                cause: Box::new(e),
            }),
            entry => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::String,
                actual: entry.r#type(),
            }),
        }
    }

    /// Get a copy of the string literal at index `idx`.
    pub fn get_owned_string(&self, idx: u16) -> CPAccessResult<String> {
        self.get_string(idx).map(str::to_string)
    }

    /// Get the fully-qualified class name at index `idx`.
    pub fn get_class_name(&self, idx: u16) -> CPAccessResult<QualifiedClassName> {
        match self.get(idx)? {
            &CPEntry::Class(idx) => {
                let s = self.get_utf8(idx).map_err(|e| CPAccessError::BadTarget {
                    request: idx,
                    cause: Box::new(e),
                })?;
                QualifiedClassName::from_full_str(s).map_err(|e| {
                    CPAccessError::FailedTypeConversion {
                        request: idx,
                        source: CPEntryType::Utf8,
                        cause: Some(Box::new(NomFlatError::from(e))),
                    }
                })
            }
            entry => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Class,
                actual: entry.r#type(),
            }),
        }
    }

    /// Get the type at index `idx`.
    pub fn get_type(&self, idx: u16) -> CPAccessResult<JavaType> {
        match self.get(idx)? {
            CPEntry::Utf8(s) => s
                .parse()
                .map_err(move |e| CPAccessError::FailedTypeConversion {
                    request: idx,
                    source: CPEntryType::Utf8,
                    cause: Some(Box::new(e)),
                }),
            entry => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::Utf8,
                actual: entry.r#type(),
            }),
        }
    }

    /// Get the name-type pair at index `idx`.
    pub fn get_name_and_type(&self, idx: u16) -> CPAccessResult<(JavaIdentifier, JavaType)> {
        match self.get(idx)? {
            &CPEntry::NameAndType(ident_idx, type_idx) => {
                let ident = self.get_utf8(ident_idx)?.parse().map_err(|e| {
                    CPAccessError::FailedTypeConversion {
                        request: ident_idx,
                        source: CPEntryType::Utf8,
                        cause: Some(Box::new(e)),
                    }
                })?;
                let r#type = self.get_type(type_idx)?;
                Ok((ident, r#type))
            }
            .map_err(|e| CPAccessError::BadTarget {
                request: idx,
                cause: Box::new(e),
            }),
            entry => Err(CPAccessError::BadEntryType {
                request: idx,
                expected: CPEntryType::NameAndType,
                actual: entry.r#type(),
            }),
        }
    }

    /// Get the symbolic method reference at index `idx`.
    pub fn get_method_ref(&self, idx: u16) -> CPAccessResult<MethodRef> {
        match *(self.get(idx)?) {
            CPEntry::Methodref(qcn_idx, nat_idx) => {
                let owner = self.get_class_name(qcn_idx)?;
                let (ident, r#type) = self.get_name_and_type(nat_idx)?;
                Ok(MethodRef::new(false, owner, ident, r#type))
            }
            .map_err(|e| CPAccessError::BadTarget {
                request: idx,
                cause: Box::new(e),
            }),
            CPEntry::InterfaceMethodref(qcn_idx, nat_idx) => {
                let owner = self.get_class_name(qcn_idx)?;
                let (ident, r#type) = self.get_name_and_type(nat_idx)?;
                Ok(MethodRef::new(true, owner, ident, r#type))
            }
            .map_err(|e| CPAccessError::BadTarget {
                request: idx,
                cause: Box::new(e),
            }),
            _ => unimplemented!("ConstantPool::get_method_ref: non-Methodref"),
        }
    }

    /// Get the symbolic field reference at index `idx`.
    pub fn get_field_ref(&self, idx: u16) -> CPAccessResult<FieldRef> {
        match self.get(idx)? {
            &CPEntry::Fieldref(qcn_idx, nat_idx) => (|| {
                let owner = self.get_class_name(qcn_idx)?;
                let (ident, r#type) = self.get_name_and_type(nat_idx)?;
                Ok(FieldRef::new(owner, ident, r#type))
            })()
            .map_err(|e| CPAccessError::BadTarget {
                request: idx,
                cause: Box::new(e),
            }),
            _ => unimplemented!("ConstantPool::get_field_ref: non-Fieldref"),
        }
    }

    /// Get the index of the first entry in the constant pool that is equal to `entry`.
    /// `CPEntry::After8Byte` is never equal to any constant pool entry.
    fn index_of(&self, entry: &CPEntry) -> Option<u16> {
        if &CPEntry::After8Byte == entry {
            return None;
        }
        (1..=self.size()).find(|&i| &self[i] == entry)
    }

    /// Ensure that `entry` is present in the constant pool. If `entry` is already present in the
    /// constant pool, returns the index of `entry` and does not modify `self`. Otherwise, pushes
    /// `entry` into the constant pool and returns the index it was pushed to.
    pub fn add(&mut self, entry: CPEntry) -> CPAccessResult<u16> {
        match (self.index_of(&entry), self.size()) {
            (None, size) if size as usize > Self::MAX_CAPACITY => {
                unreachable!("Should never exceed 0xFFFE entries in pool")
            }
            (None, size) if size as usize == Self::MAX_CAPACITY => {
                Err(CPAccessError::ConstantPoolFull)
            }
            (None, size) => {
                let is_wide = matches!(entry, CPEntry::Long(_) | CPEntry::Double(_));
                self.pool.push(entry);
                if is_wide {
                    self.pool.push(CPEntry::After8Byte);
                }
                Ok(size + 1)
            }
            (Some(idx), _) => Ok(idx),
        }
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_bool(&mut self, value: bool) -> CPAccessResult<u16> {
        self.add_int(if value { 1 } else { 0 })
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_byte(&mut self, value: i8) -> CPAccessResult<u16> {
        self.add_int(value.into())
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_char(&mut self, value: char) -> CPAccessResult<u16> {
        let mut shorts = [0; 2];
        let encoded_shorts = value.encode_utf16(&mut shorts);
        let int_value = if encoded_shorts.len() == 2 {
            let msb = encoded_shorts[0].to_be_bytes();
            let lsb = encoded_shorts[1].to_be_bytes();
            i32::from_be_bytes([msb[0], msb[1], lsb[0], lsb[1]])
        } else {
            i32::from(encoded_shorts[0] as i16)
        };
        self.add_int(int_value)
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_short(&mut self, value: i16) -> CPAccessResult<u16> {
        self.add_int(value.into())
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_int(&mut self, value: i32) -> CPAccessResult<u16> {
        self.add(CPEntry::Integer(value))
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_long(&mut self, value: i64) -> CPAccessResult<u16> {
        self.add(CPEntry::Long(value))
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_float(&mut self, value: f32) -> CPAccessResult<u16> {
        self.add(CPEntry::Float(value))
    }

    /// Ensure that `value` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_double(&mut self, value: f64) -> CPAccessResult<u16> {
        self.add(CPEntry::Double(value))
    }

    /// Ensure that `s` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_utf8(&mut self, s: String) -> CPAccessResult<u16> {
        self.add(CPEntry::Utf8(s))
    }

    /// Ensure that `t` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_field_type(&mut self, t: JavaFieldType) -> CPAccessResult<u16> {
        let value = format!("{t}");
        self.add_utf8(value)
    }

    /// Ensure that `s` is present in the constant pool as a string literal. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_string(&mut self, s: String) -> CPAccessResult<u16> {
        let utf8_idx = self.add_utf8(s)?;
        self.add(CPEntry::String(utf8_idx))
    }

    /// Ensure that `name` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_class_name(&mut self, name: QualifiedClassName) -> CPAccessResult<u16> {
        let utf8_idx = self.add_utf8(name.to_internal_form(false))?;
        self.add(CPEntry::Class(utf8_idx))
    }

    /// Ensure that the string representation of `type` is present in the constant pool. See
    /// [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_type(&mut self, r#type: JavaType) -> CPAccessResult<u16> {
        self.add_utf8(r#type.to_internal_form())
    }

    /// Ensure that `(name, type)` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_name_and_type(
        &mut self,
        name: JavaIdentifier,
        r#type: JavaType,
    ) -> CPAccessResult<u16> {
        let ident_idx = self.add_utf8(name.to_string())?;
        let type_idx = self.add_type(r#type)?;
        self.add(CPEntry::NameAndType(ident_idx, type_idx))
    }

    /// Ensure that `method_ref` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_method_ref(&mut self, method_ref: MethodRef) -> CPAccessResult<u16> {
        let qcn_idx = self.add_class_name(method_ref.owner())?;
        let nat_idx = self.add_name_and_type(method_ref.ident(), method_ref.r#type())?;
        if method_ref.is_interface_method() {
            self.add(CPEntry::InterfaceMethodref(qcn_idx, nat_idx))
        } else {
            self.add(CPEntry::Methodref(qcn_idx, nat_idx))
        }
    }

    /// Ensure that `field_ref` is present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_field_ref(&mut self, field_ref: FieldRef) -> CPAccessResult<u16> {
        let qcn_idx = self.add_class_name(field_ref.owner())?;
        let nat_idx = self.add_name_and_type(field_ref.ident(), field_ref.r#type())?;
        self.add(CPEntry::Fieldref(qcn_idx, nat_idx))
    }

    /// Ensure that `interfaces` are all present in the constant pool. See [`add`].
    ///
    /// [`add`]: #method.add
    pub fn add_interfaces(
        &mut self,
        interfaces: Vec<QualifiedClassName>,
    ) -> CPAccessResult<Vec<u16>> {
        interfaces
            .into_iter()
            .map(|interface| self.add_type(JavaType::Class(interface)))
            .collect()
    }
}

impl Index<u16> for ConstantPool {
    type Output = CPEntry;

    fn index(&self, idx: u16) -> &Self::Output {
        self.get(idx)
            .unwrap_or_else(|_| panic!("Index {} out of bounds: [1, {}]", idx, self.size()))
    }
}
