use crate::{
    types::{
        field::{ClassType, JavaFieldType},
        JavaPrimitive, JavaType,
    },
    ClassParseError, CrateResult, Either, JavaAttribute,
};

use std::{collections::HashMap, io::Read};

use super::{
    constant_pool::{CPAccessError, CPEntryType},
    raw::annotation::{RawAnnotation, RawAnnotationElement},
    read_u16, read_u8, ConstantPool,
};

/// An annotation on an item.
#[derive(Clone, Debug, PartialEq)]
pub struct Annotation {
    /// The type of the annotation.
    r#type: ClassType,
    /// The arguments to the annotation.
    kv_pairs: HashMap<String, AnnotationElement>,
}

impl Annotation {
    /// Read an annotation from the compiled class file backing `src`.
    pub(super) fn read(
        src: &mut dyn Read,
        pool: &ConstantPool,
        counter: &mut usize,
    ) -> CrateResult<Self> {
        let type_idx = read_u16(src, counter)?;
        let r#type = match pool.get_field_type(type_idx)? {
            JavaFieldType::Class(c) => c,
            _ => Err(CPAccessError::FailedTypeConversion {
                request: type_idx,
                source: CPEntryType::Utf8,
                cause: None,
            })?,
        };
        let num_kv_pairs = read_u16(src, counter)?;
        let kv_pairs = (0..num_kv_pairs)
            .map(|_| {
                let name_idx = read_u16(src, counter)?;
                let name = pool.get_utf8(name_idx)?.to_owned();
                let value = AnnotationElement::read(src, pool, counter)?;
                Ok((name, value))
            })
            .collect::<CrateResult<HashMap<_, _>>>()?;
        Ok(Self { r#type, kv_pairs })
    }

    /// Convert the annotation to a form that can be written to a class file without any further
    /// modifications of the constant pool.
    pub(super) fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawAnnotation> {
        let type_idx = pool.add_field_type(JavaFieldType::Class(self.r#type))?;
        let kv_pairs = self
            .kv_pairs
            .into_iter()
            .map(|(key, value)| {
                let name_idx = pool.add_utf8(key)?;
                let value = value.into_raw(pool)?;
                Ok((name_idx, value))
            })
            .collect::<CrateResult<HashMap<_, _>>>()?;
        Ok(RawAnnotation { type_idx, kv_pairs })
    }
}

/// An argument to an annotation.
#[derive(Clone, Debug, PartialEq)]
pub enum AnnotationElement {
    /// A boolean value.
    Bool(bool),
    /// An 8-bit signed integral value.
    Byte(i8),
    /// A character value.
    Char(char),
    /// A 16-bit signed integral value.
    Short(i16),
    /// A 32-bit signed integral value.
    Int(i32),
    /// A 64-bit signed integral value.
    Long(i64),
    /// A 32-bit floating-point value.
    Float(f32),
    /// A 64-bit floating-point value.
    Double(f64),
    /// A string literal.
    String(String),
    /// An enum variant.
    Enum {
        /// The name of the type that the enum variant belongs to.
        r#type: JavaFieldType,
        /// The simple name of the enum variant.
        value_name: String,
    },
    /// The (possibly unboxed) type `T` in `Class<T>`.
    Class(Either<JavaFieldType, JavaPrimitive>),
    /// A nested annotation.
    Annotation(Annotation),
    /// An array of values. The Java language requires that these values all have the same type and
    /// not be arrays.
    Array(Vec<Self>),
}

impl AnnotationElement {
    /// Read an annotation argument value from the compiled class file backing `src`.
    pub(super) fn read(
        src: &mut dyn Read,
        pool: &ConstantPool,
        counter: &mut usize,
    ) -> CrateResult<Self> {
        match read_u8(src, counter)? {
            b'B' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_byte(value_idx)?;
                Ok(Self::Byte(value))
            }
            b'C' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_char(value_idx)?;
                Ok(Self::Char(value))
            }
            b'D' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_double(value_idx)?;
                Ok(Self::Double(value))
            }
            b'F' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_float(value_idx)?;
                Ok(Self::Float(value))
            }
            b'I' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_int(value_idx)?;
                Ok(Self::Int(value))
            }
            b'J' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_long(value_idx)?;
                Ok(Self::Long(value))
            }
            b'S' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_short(value_idx)?;
                Ok(Self::Short(value))
            }
            b'Z' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_bool(value_idx)?;
                Ok(Self::Bool(value))
            }
            b's' => {
                let value_idx = read_u16(src, counter)?;
                let value = pool.get_owned_string(value_idx)?;
                Ok(Self::String(value))
            }
            b'e' => {
                let type_name_idx = read_u16(src, counter)?;
                let const_name_idx = read_u16(src, counter)?;
                let r#type = pool.get_field_type(type_name_idx)?;
                let value_name = pool.get_utf8(const_name_idx)?.to_owned();
                Ok(Self::Enum { r#type, value_name })
            }
            b'c' => {
                let class_info_idx = read_u16(src, counter)?;
                let field_type = match pool.get_field_type(class_info_idx) {
                    Ok(r#type) => Either::Left(r#type),
                    Err(_) => match pool.get_type(class_info_idx)? {
                        JavaType::Primitive(p) => Either::Right(p),
                        _ => Err(CPAccessError::FailedTypeConversion {
                            request: class_info_idx,
                            source: CPEntryType::Utf8,
                            cause: None,
                        })?,
                    },
                };
                Ok(Self::Class(field_type))
            }
            b'@' => Ok(Self::Annotation(Annotation::read(src, pool, counter)?)),
            b'[' => {
                let num_values = read_u16(src, counter)?;
                let values = (0..num_values)
                    .map(|_| Self::read(src, pool, counter))
                    .collect::<CrateResult<Vec<Self>>>()?;
                Ok(Self::Array(values))
            }
            tag => Err(ClassParseError::InvalidAttributeValue {
                name: format!(
                    "{}.annotations[?].element_value_pairs[?].tag",
                    JavaAttribute::RUNTIME_VISIBLE_ANNOTATIONS
                ),
                value: format!("{tag:?}"),
            }
            .into()),
        }
    }

    /// Convert the annotation argument value to a form that can be written to a class file without
    /// any further modifications of the constant pool.
    pub(super) fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawAnnotationElement> {
        match self {
            Self::Bool(b) => {
                let const_value_idx = pool.add_bool(b)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'Z',
                    const_value_idx,
                })
            }
            Self::Byte(b) => {
                let const_value_idx = pool.add_byte(b)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'B',
                    const_value_idx,
                })
            }
            Self::Char(c) => {
                let const_value_idx = pool.add_char(c)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'C',
                    const_value_idx,
                })
            }
            Self::Short(s) => {
                let const_value_idx = pool.add_short(s)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'S',
                    const_value_idx,
                })
            }
            Self::Int(i) => {
                let const_value_idx = pool.add_int(i)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'I',
                    const_value_idx,
                })
            }
            Self::Long(l) => {
                let const_value_idx = pool.add_long(l)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'J',
                    const_value_idx,
                })
            }
            Self::Float(f) => {
                let const_value_idx = pool.add_float(f)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'F',
                    const_value_idx,
                })
            }
            Self::Double(d) => {
                let const_value_idx = pool.add_double(d)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b'D',
                    const_value_idx,
                })
            }
            Self::String(s) => {
                let const_value_idx = pool.add_utf8(s)?;
                Ok(RawAnnotationElement::Primitive {
                    tag: b's',
                    const_value_idx,
                })
            }
            Self::Enum { r#type, value_name } => {
                let type_name_idx = pool.add_field_type(r#type)?;
                let const_name_idx = pool.add_utf8(value_name)?;
                Ok(RawAnnotationElement::Enum {
                    type_name_idx,
                    const_name_idx,
                })
            }
            Self::Class(Either::Left(jft)) => {
                let class_info_idx = pool.add_field_type(jft)?;
                Ok(RawAnnotationElement::Class { class_info_idx })
            }
            Self::Class(Either::Right(jp)) => {
                let class_info_idx = pool.add_type(JavaType::Primitive(jp))?;
                Ok(RawAnnotationElement::Class { class_info_idx })
            }
            Self::Annotation(a) => {
                let annotation = a.into_raw(pool)?;
                Ok(RawAnnotationElement::Annotation(annotation))
            }
            Self::Array(vs) => {
                let vs = vs
                    .into_iter()
                    .map(|v| v.into_raw(pool))
                    .collect::<CrateResult<_>>()?;
                Ok(RawAnnotationElement::Array(vs))
            }
        }
    }
}
