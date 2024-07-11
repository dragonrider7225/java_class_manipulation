use std::{collections::HashMap, io::Write};

use extended_io as eio;

#[cfg(doc)]
use crate::fragment::{annotation::Annotation, ConstantPool};
use crate::CrateResult;

/// A version of [`Annotation`] that indexes into some [`ConstantPool`] as specified by [the
/// JVMS](https://docs.oracle.com/javase/specs/jvms/se7/html/index.html).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RawAnnotation {
    /// The index in the constant pool of the type of this annotation.
    pub(in crate::fragment) type_idx: u16,
    /// The arguments to this annotation. The keys are the indices in the constant pool of the
    /// names of the arguments.
    pub(in crate::fragment) kv_pairs: HashMap<u16, RawAnnotationElement>,
}

impl RawAnnotation {
    /// The number of bytes that will be written by [`write()`].
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        2 + 2
            + self
                .kv_pairs
                .values()
                .map(|value| 2 + value.len())
                .sum::<usize>()
    }

    /// Write this annotation to the class file backing `sink`.
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.type_idx)?;
        eio::write_u16(sink, u16::try_from(self.kv_pairs.len())?)?;
        self.kv_pairs.into_iter().try_for_each(|(key, value)| {
            eio::write_u16(sink, key)?;
            value.write(sink)
        })
    }
}

/// An argument to an annotation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RawAnnotationElement {
    /// A value of a primitive type (including string literals).
    Primitive {
        /// What primitive type is this value.
        tag: u8,
        /// The index of this value in the constant pool.
        const_value_idx: u16,
    },
    /// An enum constant.
    Enum {
        /// The index of the name of the enum in the constant pool.
        type_name_idx: u16,
        /// The index of the name of the constant in the constant pool.
        const_name_idx: u16,
    },
    /// A value of type `Class`.
    Class {
        /// The index in the constant pool of the possibly-generic internal form of the `T` in
        /// `Class<T>`.
        class_info_idx: u16,
    },
    /// A nested annotation.
    Annotation(RawAnnotation),
    /// An array argument. The Java language requires that the elements of the array are all of the
    /// same non-array type.
    Array(Vec<RawAnnotationElement>),
}

impl RawAnnotationElement {
    /// The number of bytes that will be written by [`write()`].
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        1 + match self {
            Self::Primitive { .. } => 2,
            Self::Enum { .. } => 4,
            Self::Class { .. } => 2,
            Self::Annotation(annotation_value) => annotation_value.len(),
            Self::Array(vs) => 2 + vs.iter().map(Self::len).sum::<usize>(),
        }
    }

    /// Write this annotation argument to the class file backing `sink`.
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        match self {
            Self::Primitive {
                tag,
                const_value_idx,
            } => {
                eio::write_u8(sink, tag)?;
                eio::write_u16(sink, const_value_idx)?
            }
            Self::Enum {
                type_name_idx,
                const_name_idx,
            } => {
                eio::write_u8(sink, b'e')?;
                eio::write_u16(sink, type_name_idx)?;
                eio::write_u16(sink, const_name_idx)?
            }
            Self::Class { class_info_idx } => {
                eio::write_u8(sink, b'c')?;
                eio::write_u16(sink, class_info_idx)?
            }
            Self::Annotation(a) => {
                eio::write_u8(sink, b'@')?;
                a.write(sink)?
            }
            Self::Array(vs) => {
                eio::write_u8(sink, b'[')?;
                eio::write_u16(sink, u16::try_from(vs.len())?)?;
                vs.into_iter().try_for_each(|v| v.write(sink))?
            }
        }
        Ok(())
    }
}
