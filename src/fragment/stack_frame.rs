use std::io::{self, Read, Write};

use crate::{types::QualifiedClassName, CrateError, CrateResult};

use super::ConstantPool;

/// A stack map frame.
pub trait Frame {
    /// The tag of the frame.
    fn tag(&self) -> u8;

    /// The position of the frame relative to the previous frame.
    fn offset_delta(&self) -> u16;
}

/// A writable stack map frame.
#[allow(clippy::len_without_is_empty)]
pub trait WritableFrame: Frame {
    /// The number of bytes that will be written by a successful call to [`write()`].
    fn len(&self) -> usize;

    /// Writes the frame to `sink`.
    fn write(&self, sink: &mut dyn Write) -> io::Result<()>;
}

/// An owned stack map frame.
#[derive(Clone, Debug)]
pub enum StackMapFrame {
    #[allow(missing_docs)]
    SameFrame(SameFrame),
    #[allow(missing_docs)]
    SameLocals1StackItemFrame(SameLocals1StackItemFrame),
    #[allow(missing_docs)]
    SameLocals1StackItemFrameExtended(SameLocals1StackItemFrameExtended),
    #[allow(missing_docs)]
    ChopFrame(ChopFrame),
    #[allow(missing_docs)]
    SameFrameExtended(SameFrameExtended),
    #[allow(missing_docs)]
    AppendFrame(AppendFrame),
    #[allow(missing_docs)]
    FullFrame(FullFrame),
}

impl StackMapFrame {
    /// Reads a single stack map frame from `src`. `*counter` is incremented by the total number of
    /// bytes read.
    pub fn read(src: &mut dyn Read, pool: &ConstantPool, counter: &mut usize) -> CrateResult<Self> {
        let tag = super::read_u8(src, counter)?;
        match tag {
            tag @ 0..64 => Ok(Self::SameFrame(SameFrame { offset_delta: tag })),
            tag @ 64..128 => {
                let vti = VerificationTypeInfo::read(src, pool, counter)?;
                Ok(Self::SameLocals1StackItemFrame(SameLocals1StackItemFrame {
                    offset_delta: tag - 64,
                    stack: vti,
                }))
            }
            tag @ 128..247 => Err(format!("Unknown frame tag: {tag}").into()),
            247 => {
                let offset_delta = super::read_u16(src, counter)?;
                let vti = VerificationTypeInfo::read(src, pool, counter)?;
                Ok(Self::SameLocals1StackItemFrameExtended(
                    SameLocals1StackItemFrameExtended {
                        offset_delta,
                        stack: vti,
                    },
                ))
            }
            tag @ 248..251 => {
                let offset_delta = super::read_u16(src, counter)?;
                Ok(Self::ChopFrame(ChopFrame {
                    num_dropped_locals: 251 - tag,
                    offset_delta,
                }))
            }
            251 => {
                let offset_delta = super::read_u16(src, counter)?;
                Ok(Self::SameFrameExtended(SameFrameExtended { offset_delta }))
            }
            tag @ 252..255 => {
                let num_extra_locals = tag - 251;
                let offset_delta = super::read_u16(src, counter)?;
                let locals = (0..num_extra_locals)
                    .map(|_| VerificationTypeInfo::read(src, pool, counter))
                    .collect::<CrateResult<_>>()?;
                Ok(Self::AppendFrame(AppendFrame {
                    offset_delta,
                    locals,
                }))
            }
            255 => {
                let offset_delta = super::read_u16(src, counter)?;
                let num_locals = super::read_u16(src, counter)?;
                let locals = (0..num_locals)
                    .map(|_| VerificationTypeInfo::read(src, pool, counter))
                    .collect::<CrateResult<_>>()?;
                let num_stack_items = super::read_u16(src, counter)?;
                let stack = (0..num_stack_items)
                    .map(|_| VerificationTypeInfo::read(src, pool, counter))
                    .collect::<CrateResult<_>>()?;
                Ok(Self::FullFrame(FullFrame {
                    offset_delta,
                    locals,
                    stack,
                }))
            }
        }
    }

    /// Converts the stack map frame into a form that can be written without further modifying the
    /// constant pool.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawStackMapFrame> {
        match self {
            Self::SameFrame(SameFrame { offset_delta }) => {
                Ok(RawStackMapFrame::SameFrame(RawSameFrame {
                    frame_type: offset_delta,
                }))
            }
            Self::SameLocals1StackItemFrame(SameLocals1StackItemFrame {
                offset_delta,
                stack,
            }) => Ok(RawStackMapFrame::SameLocals1StackItemFrame(
                RawSameLocals1StackItemFrame {
                    frame_type: offset_delta + 64,
                    stack: stack.into_raw(pool)?,
                },
            )),
            Self::SameLocals1StackItemFrameExtended(SameLocals1StackItemFrameExtended {
                offset_delta,
                stack,
            }) => Ok(RawStackMapFrame::SameLocals1StackItemFrameExtended(
                RawSameLocals1StackItemFrameExtended {
                    offset_delta,
                    stack: stack.into_raw(pool)?,
                },
            )),
            Self::ChopFrame(ChopFrame {
                num_dropped_locals,
                offset_delta,
            }) => Ok(RawStackMapFrame::ChopFrame(RawChopFrame {
                frame_type: 251 - num_dropped_locals,
                offset_delta,
            })),
            Self::SameFrameExtended(SameFrameExtended { offset_delta }) => {
                Ok(RawStackMapFrame::SameFrameExtended(RawSameFrameExtended {
                    offset_delta,
                }))
            }
            Self::AppendFrame(AppendFrame {
                offset_delta,
                locals,
            }) => Ok(RawStackMapFrame::AppendFrame(RawAppendFrame {
                frame_type: u8::try_from(locals.len())? + 251,
                offset_delta,
                locals: locals
                    .into_iter()
                    .map(|vti| vti.into_raw(pool))
                    .collect::<CrateResult<_>>()?,
            })),
            Self::FullFrame(FullFrame {
                offset_delta,
                locals,
                stack,
            }) => Ok(RawStackMapFrame::FullFrame(RawFullFrame {
                offset_delta,
                locals: locals
                    .into_iter()
                    .map(|vti| vti.into_raw(pool))
                    .collect::<CrateResult<_>>()?,
                stack: stack
                    .into_iter()
                    .map(|vti| vti.into_raw(pool))
                    .collect::<CrateResult<_>>()?,
            })),
        }
    }
}

impl Frame for StackMapFrame {
    fn tag(&self) -> u8 {
        match self {
            Self::SameFrame(frame) => frame.tag(),
            Self::SameLocals1StackItemFrame(frame) => frame.tag(),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.tag(),
            Self::ChopFrame(frame) => frame.tag(),
            Self::SameFrameExtended(frame) => frame.tag(),
            Self::AppendFrame(frame) => frame.tag(),
            Self::FullFrame(frame) => frame.tag(),
        }
    }

    fn offset_delta(&self) -> u16 {
        match self {
            Self::SameFrame(frame) => frame.offset_delta(),
            Self::SameLocals1StackItemFrame(frame) => frame.offset_delta(),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.offset_delta(),
            Self::ChopFrame(frame) => frame.offset_delta(),
            Self::SameFrameExtended(frame) => frame.offset_delta(),
            Self::AppendFrame(frame) => frame.offset_delta(),
            Self::FullFrame(frame) => frame.offset_delta(),
        }
    }
}

/// A frame that has exactly the same local variables as the previous frame and has an empty
/// operand stack. The `offset_delta` must be in the range `0..64` and is identical to the `tag`.
#[derive(Clone, Copy, Debug)]
pub struct SameFrame {
    /// The position of this frame relative to the previous frame.
    offset_delta: u8,
}

impl Frame for SameFrame {
    fn tag(&self) -> u8 {
        self.offset_delta
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta.into()
    }
}

impl From<SameFrame> for u8 {
    fn from(value: SameFrame) -> Self {
        value.offset_delta
    }
}

impl TryFrom<u8> for SameFrame {
    type Error = CrateError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < 64 {
            Ok(Self {
                offset_delta: value,
            })
        } else {
            let _ = u8::try_from(256)?;
            unreachable!("256 does not fit into a u8")
        }
    }
}

/// A frame which has the same local variables as the previous frame and exactly one entry in the
/// operand stack. The `offset_delta` must be in the range `0..64` and is exactly 64 less than the
/// `tag`.
#[derive(Clone, Debug)]
pub struct SameLocals1StackItemFrame {
    /// The offset of the frame relative to the previous frame. In the range 0..64.
    offset_delta: u8,
    /// The type of the single entry in the operand stack.
    pub stack: VerificationTypeInfo,
}

impl SameLocals1StackItemFrame {
    /// Creates a new `SameLocals1StackItemFrame`. `offset_delta` must be in the range `0..64`.
    pub fn new(offset_delta: u16, stack: VerificationTypeInfo) -> CrateResult<Self> {
        if offset_delta < 64 {
            let offset_delta = offset_delta as _;
            Ok(Self {
                offset_delta,
                stack,
            })
        } else {
            let _ = u8::try_from(256)?;
            unreachable!("256 doesn't fit into a u8")
        }
    }
}

impl Frame for SameLocals1StackItemFrame {
    fn tag(&self) -> u8 {
        self.offset_delta + 64
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta.into()
    }
}

/// Like [`SameLocals1StackItemFrame`] except that `offset_delta` is not restricted to the range
/// `0..64`.
#[derive(Clone, Debug)]
pub struct SameLocals1StackItemFrameExtended {
    /// The position of this frame relative to the previous frame.
    pub offset_delta: u16,
    /// The type of the single entry in the operand stack.
    pub stack: VerificationTypeInfo,
}

impl Frame for SameLocals1StackItemFrameExtended {
    fn tag(&self) -> u8 {
        247
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

/// A frame that drops the last one to three local variables and has an empty operand stack.
#[derive(Clone, Debug)]
pub struct ChopFrame {
    /// The number of local variables to drop.
    num_dropped_locals: u8,
    /// The position of this frame relative to the previous frame.
    pub offset_delta: u16,
}

impl ChopFrame {
    /// Creates a new chop frame. `num_dropped_locals` must be in the range `1..4`.
    pub fn new(num_dropped_locals: u8, offset_delta: u16) -> CrateResult<Self> {
        if 0 < num_dropped_locals && num_dropped_locals < 4 {
            Ok(Self {
                num_dropped_locals,
                offset_delta,
            })
        } else {
            let _ = u8::try_from(256)?;
            unreachable!("256 doesn't fit in a u8")
        }
    }
}

impl Frame for ChopFrame {
    fn tag(&self) -> u8 {
        251 - self.num_dropped_locals
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

/// Like [`SameFrame`] except that the `offset_delta` is not restricted to the range `0..64`.
#[derive(Clone, Debug)]
pub struct SameFrameExtended {
    /// The position of this frame relative to the previous frame.
    pub offset_delta: u16,
}

impl Frame for SameFrameExtended {
    fn tag(&self) -> u8 {
        251
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

/// An append frame has an empty operand stack and the same local variables as the previous frame
/// except that an extra one to three local variables are added.
#[derive(Clone, Debug)]
pub struct AppendFrame {
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
    /// The types of the new local variables.
    locals: Vec<VerificationTypeInfo>,
}

impl AppendFrame {
    /// Creates a new append frame with the given offset and new local variables.
    pub fn new(offset_delta: u16, locals: Vec<VerificationTypeInfo>) -> CrateResult<Self> {
        let num_extra_locals = locals.len();
        if num_extra_locals == 0 || 4 <= num_extra_locals {
            let _ = u8::try_from(256)?;
            unreachable!("256 doesn't fit in a u8")
        } else {
            Ok(Self {
                offset_delta,
                locals,
            })
        }
    }
}

impl Frame for AppendFrame {
    fn tag(&self) -> u8 {
        self.locals.len() as u8 + 251
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

/// A full frame completely redefines the local variables and operand stack.
#[derive(Clone, Debug)]
pub struct FullFrame {
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
    /// The types of the local variables in this frame.
    locals: Vec<VerificationTypeInfo>,
    /// The types of the values on the operand stack from bottom to top.
    stack: Vec<VerificationTypeInfo>,
}

impl FullFrame {
    /// Creates a new full frame. The number of local variables and the size of the stack must both
    /// fit into a `u16`.
    pub fn new(
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    ) -> CrateResult<Self> {
        if locals.len() > usize::from(u16::MAX) {
            Err(format!("Too many local variables: {}", locals.len()).into())
        } else if stack.len() > usize::from(u16::MAX) {
            Err(format!("Too many elements in operand stack: {}", stack.len()).into())
        } else {
            Ok(Self {
                offset_delta,
                locals,
                stack,
            })
        }
    }
}

impl Frame for FullFrame {
    fn tag(&self) -> u8 {
        255
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

/// A type used by the type checker.
#[derive(Clone, Debug)]
pub enum VerificationTypeInfo {
    /// Top is the universal supertype.
    Top,
    /// Integer is a direct subtype of top and represents all integer types that fit into a single
    /// 4-byte word.
    Integer,
    /// Float is a direct subtype of top.
    Float,
    /// Double is a direct subtype of top.
    ///
    /// Double and Long take up two slots, and as such, each imposes restrictions on a second slot
    /// as follows:
    ///
    /// * If the value is a local variable, the next slot following this one must be of type Top
    /// * If the value is on the stack, the next slot up on the stack from this one must be of type
    ///   Top.
    Double,
    /// Long is a direct subtype of top.
    ///
    /// Double and Long take up two slots, and as such, each imposes restrictions on a second slot
    /// as follows:
    ///
    /// * If the value is a local variable, the next slot following this one must be of type Top
    /// * If the value is on the stack, the next slot up on the stack from this one must be of type
    ///   Top.
    Long,
    /// Null is a subtype of any class or array type.
    Null,
    /// UninitializedThis is a direct subtype of top.
    UninitializedThis,
    /// Object(c1) is a subtype of Object(c2) whenever c1 inherits directly or indirectly from c2.
    Object(QualifiedClassName),
    /// Uninitialized is a direct subtype of top.
    Uninitialized {
        /// The offset, in the code array of the Code attribute that contains this StackMapTable
        /// attribute, of the new instruction (§new) that created the object being stored in the
        /// location.
        offset: u16,
    },
}

impl VerificationTypeInfo {
    /// Reads a single type from `src`. Increments `counter` by the number of bytes read.
    pub fn read(src: &mut dyn Read, pool: &ConstantPool, counter: &mut usize) -> CrateResult<Self> {
        let tag = super::read_u8(src, counter)?;
        match tag {
            0 => Ok(Self::Top),
            1 => Ok(Self::Integer),
            2 => Ok(Self::Float),
            3 => Ok(Self::Double),
            4 => Ok(Self::Long),
            5 => Ok(Self::Null),
            6 => Ok(Self::UninitializedThis),
            7 => {
                let idx = super::read_u16(src, counter)?;
                let qcn = pool.get_class_name(idx)?;
                Ok(Self::Object(qcn))
            }
            8 => {
                let offset = super::read_u16(src, counter)?;
                Ok(Self::Uninitialized { offset })
            }
            tag => Err(format!("Unknown type id: {tag}").into()),
        }
    }

    /// The tag of the given type.
    pub fn tag(&self) -> u8 {
        match self {
            Self::Top => 0,
            Self::Integer => 1,
            Self::Float => 2,
            Self::Double => 3,
            Self::Long => 4,
            Self::Null => 5,
            Self::UninitializedThis => 6,
            Self::Object { .. } => 7,
            Self::Uninitialized { .. } => 8,
        }
    }

    /// Converts the type to a form that can be written as part of a class file without further
    /// modifying the constant pool.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawVerificationTypeInfo> {
        match self {
            Self::Top => Ok(RawVerificationTypeInfo::Top),
            Self::Integer => Ok(RawVerificationTypeInfo::Integer),
            Self::Float => Ok(RawVerificationTypeInfo::Float),
            Self::Double => Ok(RawVerificationTypeInfo::Double),
            Self::Long => Ok(RawVerificationTypeInfo::Long),
            Self::Null => Ok(RawVerificationTypeInfo::Null),
            Self::UninitializedThis => Ok(RawVerificationTypeInfo::UninitializedThis),
            Self::Object(name) => {
                let cpool_idx = pool.add_class_name(name)?;
                Ok(RawVerificationTypeInfo::Object { cpool_idx })
            }
            Self::Uninitialized { offset } => Ok(RawVerificationTypeInfo::Uninitialized { offset }),
        }
    }
}

/// A writable form of [`StackMapFrame`].
#[derive(Clone, Debug)]
pub enum RawStackMapFrame {
    #[allow(missing_docs)]
    SameFrame(RawSameFrame),
    #[allow(missing_docs)]
    SameLocals1StackItemFrame(RawSameLocals1StackItemFrame),
    #[allow(missing_docs)]
    SameLocals1StackItemFrameExtended(RawSameLocals1StackItemFrameExtended),
    #[allow(missing_docs)]
    ChopFrame(RawChopFrame),
    #[allow(missing_docs)]
    SameFrameExtended(RawSameFrameExtended),
    #[allow(missing_docs)]
    AppendFrame(RawAppendFrame),
    #[allow(missing_docs)]
    FullFrame(RawFullFrame),
}

impl Frame for RawStackMapFrame {
    fn tag(&self) -> u8 {
        match self {
            Self::SameFrame(frame) => frame.tag(),
            Self::SameLocals1StackItemFrame(frame) => frame.tag(),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.tag(),
            Self::ChopFrame(frame) => frame.tag(),
            Self::SameFrameExtended(frame) => frame.tag(),
            Self::AppendFrame(frame) => frame.tag(),
            Self::FullFrame(frame) => frame.tag(),
        }
    }

    fn offset_delta(&self) -> u16 {
        match self {
            Self::SameFrame(frame) => frame.offset_delta(),
            Self::SameLocals1StackItemFrame(frame) => frame.offset_delta(),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.offset_delta(),
            Self::ChopFrame(frame) => frame.offset_delta(),
            Self::SameFrameExtended(frame) => frame.offset_delta(),
            Self::AppendFrame(frame) => frame.offset_delta(),
            Self::FullFrame(frame) => frame.offset_delta(),
        }
    }
}

impl WritableFrame for RawStackMapFrame {
    fn len(&self) -> usize {
        match self {
            Self::SameFrame(frame) => frame.len(),
            Self::SameLocals1StackItemFrame(frame) => frame.len(),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.len(),
            Self::ChopFrame(frame) => frame.len(),
            Self::SameFrameExtended(frame) => frame.len(),
            Self::AppendFrame(frame) => frame.len(),
            Self::FullFrame(frame) => frame.len(),
        }
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        match self {
            Self::SameFrame(frame) => frame.write(sink),
            Self::SameLocals1StackItemFrame(frame) => frame.write(sink),
            Self::SameLocals1StackItemFrameExtended(frame) => frame.write(sink),
            Self::ChopFrame(frame) => frame.write(sink),
            Self::SameFrameExtended(frame) => frame.write(sink),
            Self::AppendFrame(frame) => frame.write(sink),
            Self::FullFrame(frame) => frame.write(sink),
        }
    }
}

/// The writable form of [`SameFrame`].
#[derive(Clone, Copy, Debug)]
pub struct RawSameFrame {
    /// The tag for this frame. From the range `0..64`.
    frame_type: u8,
}

impl Frame for RawSameFrame {
    fn tag(&self) -> u8 {
        self.frame_type
    }

    fn offset_delta(&self) -> u16 {
        self.frame_type.into()
    }
}

impl WritableFrame for RawSameFrame {
    fn len(&self) -> usize {
        1
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[self.frame_type])
    }
}

/// The writable form of [`SameLocals1StackItemFrame`].
#[derive(Clone, Copy, Debug)]
pub struct RawSameLocals1StackItemFrame {
    /// The tage for the frame. Restricted to the range `64..128`.
    frame_type: u8,
    /// The type of the single operand stack entry.
    stack: RawVerificationTypeInfo,
}

impl Frame for RawSameLocals1StackItemFrame {
    fn tag(&self) -> u8 {
        self.frame_type
    }

    fn offset_delta(&self) -> u16 {
        u16::from(self.frame_type) - 64
    }
}

impl WritableFrame for RawSameLocals1StackItemFrame {
    fn len(&self) -> usize {
        1 + self.stack.len()
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[self.frame_type])?;
        self.stack.write(sink)
    }
}

/// The writable form of [`SameLocals1StackItemFrameExtended`].
#[derive(Clone, Copy, Debug)]
pub struct RawSameLocals1StackItemFrameExtended {
    /// The position of the frame relative to the previous frame.
    offset_delta: u16,
    /// The type of the single operand stack entry.
    stack: RawVerificationTypeInfo,
}

impl Frame for RawSameLocals1StackItemFrameExtended {
    fn tag(&self) -> u8 {
        247
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

impl WritableFrame for RawSameLocals1StackItemFrameExtended {
    fn len(&self) -> usize {
        3 + self.stack.len()
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[247])?;
        extended_io::write_u16(sink, self.offset_delta)?;
        self.stack.write(sink)
    }
}

/// The writable form of [`ChopFrame`].
#[derive(Clone, Copy, Debug)]
pub struct RawChopFrame {
    /// The tag for the frame.
    frame_type: u8,
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
}

impl Frame for RawChopFrame {
    fn tag(&self) -> u8 {
        self.frame_type
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

impl WritableFrame for RawChopFrame {
    fn len(&self) -> usize {
        3
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[self.frame_type])?;
        extended_io::write_u16(sink, self.offset_delta)
    }
}

/// The writable form of [`SameFrameExtended`]
#[derive(Clone, Copy, Debug)]
pub struct RawSameFrameExtended {
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
}

impl Frame for RawSameFrameExtended {
    fn tag(&self) -> u8 {
        251
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

impl WritableFrame for RawSameFrameExtended {
    fn len(&self) -> usize {
        3
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[251])?;
        extended_io::write_u16(sink, self.offset_delta)
    }
}

/// The writable form of [`AppendFrame`].
#[derive(Clone, Debug)]
pub struct RawAppendFrame {
    /// The tag for the frame.
    frame_type: u8,
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
    /// The types of the new local variables.
    locals: Vec<RawVerificationTypeInfo>,
}

impl Frame for RawAppendFrame {
    fn tag(&self) -> u8 {
        self.frame_type
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

impl WritableFrame for RawAppendFrame {
    fn len(&self) -> usize {
        3 + self.locals.iter().map(|vti| vti.len()).sum::<usize>()
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[self.frame_type])?;
        extended_io::write_u16(sink, self.offset_delta)?;
        self.locals.iter().try_for_each(|vti| vti.write(sink))
    }
}

/// The writable form of [`FullFrame`].
#[derive(Clone, Debug)]
pub struct RawFullFrame {
    /// The position of this frame relative to the previous frame.
    offset_delta: u16,
    /// The types of this frame's local variables.
    locals: Vec<RawVerificationTypeInfo>,
    /// The types of this frame's operand stack.
    stack: Vec<RawVerificationTypeInfo>,
}

impl Frame for RawFullFrame {
    fn tag(&self) -> u8 {
        255
    }

    fn offset_delta(&self) -> u16 {
        self.offset_delta
    }
}

impl WritableFrame for RawFullFrame {
    fn len(&self) -> usize {
        5 + self.locals.iter().map(|vti| vti.len()).sum::<usize>()
            + 2
            + self.stack.iter().map(|vti| vti.len()).sum::<usize>()
    }

    fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        sink.write_all(&[255])?;
        extended_io::write_u16(sink, self.offset_delta)?;
        extended_io::write_u16(sink, self.locals.len() as _)?;
        self.locals.iter().try_for_each(|vti| vti.write(sink))?;
        extended_io::write_u16(sink, self.stack.len() as _)?;
        self.stack.iter().try_for_each(|vti| vti.write(sink))
    }
}

/// The writable form of [`VerificationTypeInfo`].
#[derive(Clone, Copy, Debug)]
pub enum RawVerificationTypeInfo {
    #[allow(missing_docs)]
    Top,
    #[allow(missing_docs)]
    Integer,
    #[allow(missing_docs)]
    Float,
    #[allow(missing_docs)]
    Double,
    #[allow(missing_docs)]
    Long,
    #[allow(missing_docs)]
    Null,
    #[allow(missing_docs)]
    UninitializedThis,
    #[allow(missing_docs)]
    Object {
        /// The index of the Class entry in the constant pool referring to the name of this type.
        cpool_idx: u16,
    },
    #[allow(missing_docs)]
    Uninitialized { offset: u16 },
}

impl RawVerificationTypeInfo {
    /// The number of bytes that will be written by [`write()`].
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        match self {
            Self::Top
            | Self::Integer
            | Self::Float
            | Self::Double
            | Self::Long
            | Self::Null
            | Self::UninitializedThis => 1,
            Self::Object { .. } | Self::Uninitialized { .. } => 3,
        }
    }

    /// Writes the type information as part of a class file.
    pub fn write(&self, sink: &mut dyn Write) -> io::Result<()> {
        match self {
            Self::Top => sink.write_all(&[0]),
            Self::Integer => sink.write_all(&[1]),
            Self::Float => sink.write_all(&[2]),
            Self::Double => sink.write_all(&[3]),
            Self::Long => sink.write_all(&[4]),
            Self::Null => sink.write_all(&[5]),
            Self::UninitializedThis => sink.write_all(&[6]),
            Self::Object { cpool_idx } => {
                sink.write_all(&[7])?;
                extended_io::write_u16(sink, *cpool_idx)
            }
            Self::Uninitialized { offset } => {
                sink.write_all(&[8])?;
                extended_io::write_u16(sink, *offset)
            }
        }
    }
}
