use extended_io as eio;

use std::{convert::TryInto, io::{self, Write}};

use crate::CrateResult;

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
    GenericAttribute {
        name_idx: u16,
        info: Vec<u8>,
    },
}

impl RawAttribute {
    pub fn name_idx(&self) -> u16 {
        match self {
            RawAttribute::ConstantValue { name_idx, .. }
            | RawAttribute::Code { name_idx, .. }
            | RawAttribute::GenericAttribute { name_idx, .. } => *name_idx,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            RawAttribute::ConstantValue { .. } => 2,
            RawAttribute::Code {
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
                    + attributes.iter().map(RawAttribute::len).sum::<usize>()
            }
            RawAttribute::GenericAttribute { info, .. } => info.len(),
        }
    }

    pub fn write(self, sink: &mut dyn Write) -> CrateResult<()> {
        eio::write_u16(sink, self.name_idx())?;
        eio::write_u32(sink, self.len().try_into()?)?;
        match self {
            RawAttribute::ConstantValue { value_idx, .. } => eio::write_u16(sink, value_idx)?,
            RawAttribute::Code {
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
                for exception_handler in exception_handlers {
                    exception_handler.write(sink)?;
                }
                eio::write_u16(sink, attributes.len().try_into()?)?;
                for attribute in attributes {
                    attribute.write(sink)?;
                }
            }
            RawAttribute::GenericAttribute { info, .. } => eio::write_byte_slice(sink, &info)?,
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
