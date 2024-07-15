use annotation::RawAnnotation;
use extended_io as eio;

use std::{
    convert::TryInto,
    io::{self, Write},
};

use crate::CrateResult;

use super::stack_frame::{RawStackMapFrame, WritableFrame as _};

pub mod annotation;

#[derive(Debug)]
pub enum RawAttribute {
    ConstantValue {
        name_idx: u16,
        value_idx: u16,
    },
    Code {
        name_idx: u16,
        max_stack: u16,
        max_locals: u16,
        body: Vec<u8>,
        exception_handlers: Vec<RawExceptionHandler>,
        attributes: Vec<RawAttribute>,
    },
    StackMapTable {
        name_idx: u16,
        entries: Vec<RawStackMapFrame>,
    },
    /// The list of exceptions that explicitly may be thrown by the function.
    Exceptions {
        /// The index of the attribute name "Exceptions" in the constant pool.
        name_idx: u16,
        /// The indices of the exception class names in the constant pool.
        exception_indices: Vec<u16>,
    },
    /// The list of non-package classes referred to by or contained in the class.
    InnerClasses {
        /// The index of the attribute name "InnerClasses" in the constant pool.
        name_idx: u16,
        /// The entries in the list of class descriptions.
        classes_info: Vec<RawInnerClassInfo>,
    },
    /// A description of the location of this local or anonymous class.
    EnclosingMethod {
        /// The index of the attribute name "EnclosingMethod" in the constant pool.
        name_idx: u16,
        /// The index in the constant pool of the class where this local or anonymous class was
        /// defined.
        class_idx: u16,
        /// The index in the constant pool of the name and type of the method where this local or
        /// anonymous class was defined, if applicable.
        method_idx: u16,
    },
    /// An alternative to the synthetic flag on classes, methods, and fields.
    Synthetic(u16),
    /// A non-erased generic type signature.
    Signature {
        /// The index of the attribute name "Signature" in the constant pool.
        name_idx: u16,
        /// The index of the type signature in the constant pool.
        signature_idx: u16,
    },
    /// The filename of the file that this class was compiled from.
    SourceFile {
        /// The index of the attribute name "SourceFile" in the constant pool.
        name_idx: u16,
        /// The index of the source file name in the constant pool.
        source_file_idx: u16,
    },
    /// Extra debug information.
    SourceDebugExtension {
        /// The index of the attribute name "SourceDebugExtension" in the constant pool.
        name_idx: u16,
        /// The number of bytes in the JVM-8 representation of `value`.
        jvm8_len: u32,
        /// The debug information.
        value: String,
    },
    /// Debug information about where in the `Code` attribute the line number of the original
    /// source code changes.
    LineNumberTable {
        /// The index of the attribute name "LineNumberTable" in the constant pool.
        name_idx: u16,
        /// The debug information.
        table: Vec<super::LineNumber>,
    },
    /// Debug information about local variables for a `Code` attribute.
    LocalVariableTable {
        /// The index of the attribute name "LocalVariableTable" in the constant pool.
        name_idx: u16,
        /// The debug information.
        table: Vec<RawLocalVariable>,
    },
    /// Debug information about local variables for a `Code` attribute.
    LocalVariableTypeTable {
        /// The index of the attribute name "LocalVariableTable" in the constant pool.
        name_idx: u16,
        /// The debug information.
        table: Vec<RawLocalVariable>,
    },
    /// A flag that the item this attribute is attached to is deprecated and should not be used.
    Deprecated {
        /// The index of the attribute name "Deprecated" in the constant pool.
        name_idx: u16,
    },
    /// The annotations on an item that are visible to the program through the reflection API.
    RuntimeVisibleAnnotations {
        /// The index of the attribute name "RuntimeVisibleAnnotations" in the constant pool.
        name_idx: u16,
        /// The annotations.
        annotations: Vec<RawAnnotation>,
    },
    /// The annotations on an item that are not visible to the program through the reflection API.
    RuntimeInvisibleAnnotations {
        /// The index of the attribute name "RuntimeInvisibleAnnotations" in the constant pool.
        name_idx: u16,
        /// The annotations.
        annotations: Vec<RawAnnotation>,
    },
    GenericAttribute {
        /// The index of the attribute's name in the constant pool.
        name_idx: u16,
        /// The raw bytes of the attribute's data.
        info: Vec<u8>,
    },
}

impl RawAttribute {
    /// The index into the constant pool of this attribute's name.
    pub fn name_idx(&self) -> u16 {
        match *self {
            Self::ConstantValue { name_idx, .. }
            | Self::Code { name_idx, .. }
            | Self::StackMapTable { name_idx, .. }
            | Self::Exceptions { name_idx, .. }
            | Self::InnerClasses { name_idx, .. }
            | Self::EnclosingMethod { name_idx, .. }
            | Self::Synthetic(name_idx)
            | Self::Signature { name_idx, .. }
            | Self::SourceFile { name_idx, .. }
            | Self::SourceDebugExtension { name_idx, .. }
            | Self::LineNumberTable { name_idx, .. }
            | Self::LocalVariableTable { name_idx, .. }
            | Self::LocalVariableTypeTable { name_idx, .. }
            | Self::Deprecated { name_idx }
            | Self::RuntimeVisibleAnnotations { name_idx, .. }
            | Self::RuntimeInvisibleAnnotations { name_idx, .. }
            | Self::GenericAttribute { name_idx, .. } => name_idx,
        }
    }

    /// The number of bytes that would be written by a successful call to [`self.write()`] minus the
    /// six bytes used for `attribute_name_index` and `attribute_length`.
    ///
    /// [`self.write()`]: #method.write
    #[allow(clippy::len_without_is_empty, reason = "This is not a container type")]
    pub fn len(&self) -> usize {
        match self {
            Self::ConstantValue { .. } => 2,
            Self::Code {
                body,
                exception_handlers,
                attributes,
                ..
            } => {
                2 + 2
                    + 4
                    + body.len()
                    + 2
                    + 8 * exception_handlers.len()
                    + 2
                    + attributes
                        .iter()
                        .map(RawAttribute::len)
                        .map(|x| x + 6)
                        .sum::<usize>()
            }
            Self::StackMapTable { entries, .. } => {
                2 + entries.iter().map(|entry| entry.len()).sum::<usize>()
            }
            Self::Exceptions {
                exception_indices, ..
            } => 2 + exception_indices.len() * 2,
            Self::InnerClasses { classes_info, .. } => 2 + classes_info.len() * 8,
            Self::EnclosingMethod { .. } => 2 + 2,
            Self::Synthetic(_) => 0,
            Self::Signature { .. } => 2,
            Self::SourceFile { .. } => 2,
            Self::SourceDebugExtension { jvm8_len, .. } => usize::try_from(*jvm8_len).unwrap(),
            Self::LineNumberTable { table, .. } => 2 + table.len() * 4,
            Self::LocalVariableTable { table, .. } | Self::LocalVariableTypeTable { table, .. } => {
                2 + table.len() * 10
            }
            Self::Deprecated { .. } => 0,
            Self::RuntimeVisibleAnnotations { annotations, .. } => {
                2 + annotations.iter().map(RawAnnotation::len).sum::<usize>()
            }
            Self::RuntimeInvisibleAnnotations { annotations, .. } => {
                2 + annotations.iter().map(RawAnnotation::len).sum::<usize>()
            }
            Self::GenericAttribute { info, .. } => info.len(),
        }
    }

    /// Writes the attribute to `sink`.
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.name_idx())?;
        eio::write_u32(sink, self.len().try_into()?)?;
        match self {
            Self::ConstantValue { value_idx, .. } => eio::write_u16(sink, value_idx)?,
            Self::Code {
                max_stack,
                max_locals,
                body,
                exception_handlers,
                attributes,
                ..
            } => {
                eio::write_u16(sink, max_stack)?;
                eio::write_u16(sink, max_locals)?;
                eio::write_u32(sink, body.len().try_into()?)?;
                eio::write_byte_slice(sink, &body)?;
                eio::write_u16(sink, exception_handlers.len().try_into()?)?;
                exception_handlers
                    .into_iter()
                    .try_for_each(|handler| handler.write(sink))?;
                eio::write_u16(sink, attributes.len().try_into()?)?;
                attributes
                    .into_iter()
                    .try_for_each(|attr| attr.write(sink))?
            }
            Self::StackMapTable { entries, .. } => {
                eio::write_u16(sink, u16::try_from(entries.len())?)?;
                entries
                    .into_iter()
                    .try_for_each(|frame| frame.write(sink))?
            }
            Self::Exceptions {
                exception_indices, ..
            } => {
                eio::write_u16(sink, u16::try_from(exception_indices.len())?)?;
                exception_indices
                    .into_iter()
                    .try_for_each(|index| eio::write_u16(sink, index))?
            }
            Self::InnerClasses { classes_info, .. } => {
                eio::write_u16(sink, u16::try_from(classes_info.len())?)?;
                classes_info
                    .into_iter()
                    .try_for_each(|info| info.write(sink))?
            }
            Self::EnclosingMethod {
                class_idx,
                method_idx,
                ..
            } => {
                eio::write_u16(sink, class_idx)?;
                eio::write_u16(sink, method_idx)?
            }
            Self::Synthetic(_) => {}
            Self::Signature { signature_idx, .. } => eio::write_u16(sink, signature_idx)?,
            Self::SourceFile {
                source_file_idx, ..
            } => eio::write_u16(sink, source_file_idx)?,
            Self::SourceDebugExtension { value, .. } => {
                let mut bytes = vec![];
                crate::write_jvm8(&mut bytes, &value)?;
                super::write_bytes(sink, &mut bytes[2..])?
            }
            Self::LineNumberTable { table, .. } => {
                eio::write_u16(sink, u16::try_from(table.len())?)?;
                table.into_iter().try_for_each(|entry| {
                    eio::write_u16(sink, entry.start_pc)?;
                    eio::write_u16(sink, entry.line_number)
                })?
            }
            Self::LocalVariableTable { table, .. } | Self::LocalVariableTypeTable { table, .. } => {
                eio::write_u16(sink, u16::try_from(table.len())?)?;
                table.into_iter().try_for_each(|entry| entry.write(sink))?
            }
            Self::Deprecated { .. } => {}
            Self::RuntimeVisibleAnnotations { annotations, .. } => {
                eio::write_u16(sink, u16::try_from(annotations.len())?)?;
                annotations
                    .into_iter()
                    .try_for_each(|annotation| annotation.write(sink))?
            }
            Self::RuntimeInvisibleAnnotations { annotations, .. } => {
                eio::write_u16(sink, u16::try_from(annotations.len())?)?;
                annotations
                    .into_iter()
                    .try_for_each(|annotation| annotation.write(sink))?
            }
            Self::GenericAttribute { info, .. } => eio::write_byte_slice(sink, &info)?,
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct RawExceptionHandler {
    /// The index in the associated "Code" attribute's code array at which this exception handler
    /// becomes active.
    pub start_pc: u16,
    /// The index in the associated "Code" attribute's code array at which this exception handler
    /// becomes inactive.
    pub end_pc: u16,
    /// The index in the associated "Code" attribute's code array to jump to if this exception
    /// handler is triggered.
    pub handler_pc: u16,
    /// The index in the constant pool of the type of exception that this exception handler can
    /// handle. If `catch_type_idx` is `0`, this exception handler can handle *all* types of
    /// exception.
    pub catch_type_idx: u16,
}

impl RawExceptionHandler {
    pub fn write(self, sink: &mut dyn Write) -> io::Result<()> {
        eio::write_u16(sink, self.start_pc)?;
        eio::write_u16(sink, self.end_pc)?;
        eio::write_u16(sink, self.handler_pc)?;
        eio::write_u16(sink, self.catch_type_idx)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct RawField {
    pub access_flags: u16,
    pub name_idx: u16,
    pub type_idx: u16,
    pub attributes: Vec<RawAttribute>,
}

impl RawField {
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.access_flags)?;
        eio::write_u16(sink, self.name_idx)?;
        eio::write_u16(sink, self.type_idx)?;
        eio::write_u16(sink, self.attributes.len().try_into()?)?;
        for attribute in self.attributes {
            attribute.write(sink)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct RawMethod {
    pub access_flags: u16,
    pub name_idx: u16,
    pub type_idx: u16,
    pub attributes: Vec<RawAttribute>,
}

impl RawMethod {
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.access_flags)?;
        eio::write_u16(sink, self.name_idx)?;
        eio::write_u16(sink, self.type_idx)?;
        eio::write_u16(sink, self.attributes.len().try_into()?)?;
        for attribute in self.attributes {
            attribute.write(sink)?;
        }
        Ok(())
    }
}

/// A description of a non-package class C referred to by or contained in another class.
#[derive(Clone, Copy, Debug)]
pub struct RawInnerClassInfo {
    /// The index of the fully-qualified class name of C in the constant pool.
    pub name_idx: u16,
    /// The index of the fully-qualified class name of the class that contains C in the constant
    /// pool.
    pub container_idx: u16,
    /// The index of the simple name of C in the constant pool.
    pub unqualified_name_idx: u16,
    /// The access flags on the class C.
    pub access_flags: u16,
}

impl RawInnerClassInfo {
    /// Writes the description to `sink`.
    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.name_idx)?;
        eio::write_u16(sink, self.container_idx)?;
        eio::write_u16(sink, self.unqualified_name_idx)?;
        eio::write_u16(sink, self.access_flags)?;
        Ok(())
    }
}

/// A local variable description in a `LocalVariableTable` or `LocalVariableTypeTable` attribute.
#[derive(Clone, Copy, Debug)]
pub struct RawLocalVariable {
    /// The index into the `Code` attribute of the first instruction where this local variable must
    /// have a value.
    pub start_pc: u16,
    /// The length of the slice into the `Code` attribute where this local variable must have a
    /// value.
    pub code_length: u16,
    /// The index in the constant pool of the name of the local variable.
    pub name_idx: u16,
    /// The index in the constant pool of the type of the local variable.
    pub type_idx: u16,
    /// The index of the local variable in the current frame.
    pub index: u16,
}

impl RawLocalVariable {
    /// Write the local variable to a class file.
    fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.start_pc)?;
        eio::write_u16(sink, self.code_length)?;
        eio::write_u16(sink, self.name_idx)?;
        eio::write_u16(sink, self.type_idx)?;
        eio::write_u16(sink, self.index)?;
        Ok(())
    }
}
