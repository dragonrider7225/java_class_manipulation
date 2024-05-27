use extended_io as eio;

use nom::{
    branch,
    bytes::complete as bytes,
    character::complete as character,
    combinator as comb,
    error::{ErrorKind, ParseError},
    multi,
    number::complete as num,
    sequence, Err, IResult, Parser,
};

use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
    io::{self, Read},
    mem, slice,
};

use crate::{
    parsers::{impl_from_str_for_nom_parse_cf, just, NomParse, NomParseContextFree},
    types::{JavaType, PrimitiveValueType, QualifiedClassName},
    AccessFlagged, ClassParseError, CrateResult, Either, FieldRef, MethodRef,
};

pub mod constant_pool;
pub use constant_pool::ConstantPool;
use constant_pool::{CPAccessError, CPAccessResult, CPEntry};

pub mod raw;
use raw::{
    RawAttribute, RawExceptionHandler, RawField, RawInnerClassInfo, RawLocalVariable, RawMethod,
};

pub mod stack_frame;
use stack_frame::StackMapFrame;

/// Reinterpret `x` as a reference to a byte slice. The length of the slice is
/// equal to the size of `T`. `T` should never be a reference type or any type
/// which contains a type which `T` should not be.
fn as_bytes<T>(x: &T) -> &[u8] {
    unsafe { slice::from_raw_parts(x as *const T as *const _, mem::size_of::<T>()) }
}

/// An exception handler for some function body.
#[derive(Clone, Debug)]
pub struct ExceptionHandler {
    /// The program counter relative to the beginning of the associated "Code" attribute's code
    /// array at which this exception handler becomes active.
    start_pc: u16,
    /// The program counter relative to the beginning of the associated "Code" attribute's code
    /// array at which this exception handler becomes inactive.
    end_pc: u16,
    /// The program counter relative to the beginning of the associated "Code" attribute's code
    /// array to jump to if this exception handler is triggered.
    handler_pc: u16,
    /// The type of exception that this exception handler can handle. If `catch_type` is
    /// `Right(())`, this exception handler can handle *all* types of exception.
    catch_type: Either<QualifiedClassName, ()>,
}

impl ExceptionHandler {
    fn read(
        src: &mut dyn Read,
        pool: &ConstantPool,
        counter: &mut usize,
    ) -> CrateResult<ExceptionHandler> {
        let start_pc = read_u16(src, counter)?;
        let end_pc = read_u16(src, counter)?;
        let handler_pc = read_u16(src, counter)?;
        let catch_type_idx = read_u16(src, counter)?;

        let catch_type = match catch_type_idx {
            0 => Either::Right(()),
            idx => Either::Left(pool.get_class_name(idx)?),
        };
        Ok(ExceptionHandler {
            start_pc,
            end_pc,
            handler_pc,
            catch_type,
        })
    }

    fn into_raw(self, pool: &mut ConstantPool) -> CPAccessResult<RawExceptionHandler> {
        let start_pc = self.start_pc;
        let end_pc = self.end_pc;
        let handler_pc = self.handler_pc;
        let catch_type_idx = match self.catch_type {
            Either::Right(()) => 0,
            Either::Left(qcn) => pool.add_class_name(qcn)?,
        };
        Ok(RawExceptionHandler {
            start_pc,
            end_pc,
            handler_pc,
            catch_type_idx,
        })
    }
}

/// Read an exception handler, resolving all references into `pool`.
pub fn read_exception_handlers(
    src: &mut dyn Read,
    pool: &ConstantPool,
    counter: &mut usize,
) -> CrateResult<Vec<ExceptionHandler>> {
    let count = read_u16(src, counter)?;
    (0..count)
        .map(|_| ExceptionHandler::read(src, pool, counter))
        .collect()
}

#[derive(Clone, Debug, PartialEq)]
pub enum JavaOpCode {
    /// No-op.
    Nop,
    /// Pushes `null` onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., null ->
    AconstNull,
    /// Pushes the `int` value -1 onto the stack. Equivalent to `bipush -1`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., -1 ->
    IconstM1,
    /// Pushes the `int` value 0 onto the stack. Equivalent to `bipush 0`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 0 ->
    Iconst0,
    /// Pushes the `int` value 1 onto the stack. Equivalent to `bipush 1`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 1 ->
    Iconst1,
    /// Pushes the `int` value 2 onto the stack. Equivalent to `bipush 2`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 2 ->
    Iconst2,
    /// Pushes the `int` value 3 onto the stack. Equivalent to `bipush 3`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 3 ->
    Iconst3,
    /// Pushes the `int` value 4 onto the stack. Equivalent to `bipush 4`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 4 ->
    Iconst4,
    /// Pushes the `int` value 5 onto the stack. Equivalent to `bipush 5`
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 5 ->
    Iconst5,
    /// Pushes the `long` value 0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 0 ->
    Lconst0,
    /// Pushes the `long` value 1 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 1 ->
    Lconst1,
    /// Pushes the `float` value 0.0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 0.0 ->
    Fconst0,
    /// Pushes the `float` value 1.0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 1.0 ->
    Fconst1,
    /// Pushes the `float` value 2.0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 2.0 ->
    Fconst2,
    /// Pushes the `double` value 0.0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 0.0 ->
    Dconst0,
    /// Pushes the `double` value 1.0 onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., 1.0 ->
    Dconst1,
    /// Sign-extends `value` to type `int` and pushes it onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Bipush { value: u8 },
    /// Pushes a 4-byte `value` from the run-time constant pool onto the stack. The constant pool
    /// entry at index `index` must be either an `int`, a `float`, a reference to a string literal,
    /// or a symbolic reference to a class, a method type, or a method handle. The resolution of
    /// the value to be pushed onto the stack is as follows:
    /// * If the constant pool entry at index `index` is an `int`, a `float`, or a `reference` to a
    ///   string literal, then the entry is pushed exactly as it appears in the constant pool.
    /// * If the constant pool entry at index `index` is a symbolic reference to a class, a method
    ///   type, or a method handle, then the named object is resolved and a `reference` to the
    ///   resolved `Object` is pushed onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Ldc { index: u8 },
    /// Like [`Ldc`], but has a 16-bit index instead of an 8-bit index.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    ///
    /// [`Ldc`]: #variant.Ldc
    LdcW { index: u16 },
    /// Pushes an 8-byte `value` onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Ldc2W {
        /// The value to push.
        value: Either<i64, f64>,
    },
    /// Pushes an `int` from a local variable onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Iload {
        /// The index into the local variable array.
        index: u8,
    },
    /// Pushes a `long` from a local variable onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Lload {
        /// The index into the local variable array.
        index: u8,
    },
    /// Pushes a `float` from a local variable onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Fload {
        /// The index into the local variable array.
        index: u8,
    },
    /// Pushes a `double` from a local variable onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Dload {
        /// The index into the local variable array.
        index: u8,
    },
    /// Pushes a `reference` from a local variable onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    Aload {
        /// The index into the local variable array.
        index: u8,
    },
    /// Like `iload 0`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Iload0,
    /// Like `iload 1`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Iload1,
    /// Like `iload 2`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Iload2,
    /// Like `iload 3`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Iload3,
    /// Like `lload 0`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Lload0,
    /// Like `lload 1`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Lload1,
    /// Like `lload 2`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Lload2,
    /// Like `lload 3`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Lload3,
    /// Like `fload 0`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Fload0,
    /// Like `fload 1`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Fload1,
    /// Like `fload 2`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Fload2,
    /// Like `fload 3`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Fload3,
    /// Like `dload 0`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Dload0,
    /// Like `dload 1`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Dload1,
    /// Like `dload 2`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Dload2,
    /// Like `dload 3`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Dload3,
    /// Like `aload 0`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    Aload0,
    /// Like `aload 1`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    Aload1,
    /// Like `aload 2`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    Aload2,
    /// Like `aload 3`.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    Aload3,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `int`. `index` must be an `int`.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Iaload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `long`. `index` must be an `int`.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Laload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `float`. `index` must be an `int`.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Faload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `double`. `index` must be an `int`.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Daload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `reference`. `index` must be an `int`.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Aaload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is either byte or boolean. `index` must be an `int`. The loaded value is
    /// sign-extended before being pushed onto the stack.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Baload,
    /// Gets the value at `arrayref[index]`. `arrayref` must be a `reference` to an array whose
    /// element type is `char`. `index` must be an `int`. The loaded value is zero-extended before
    /// being pushed onto the stack.
    /// # Operand stack:
    /// ..., arrayref, index ->
    ///
    /// ..., value ->
    Caload,
    /// Set the local variable at `index` in the local variable array to be equal to `value`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Istore {
        /// The index into the local variable array.
        index: u8,
    },
    /// Set the local variable at `index` and `index+1` in the local variable array to be equal to
    /// `value`. `value` must be of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Lstore {
        /// The index into the local variable array.
        index: u8,
    },
    /// Set the local variable at `index` in the local variable array to be equal to `value`.
    /// `value` must be of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Fstore {
        /// The index into the local variable array.
        index: u8,
    },
    /// Set the local variable at `index` in the local variable array to be equal to `value`.
    /// `value` must be of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Dstore {
        /// The index into the local variable array.
        index: u8,
    },
    /// Set the local variable at `index` in the local variable array to be equal to `objectref`.
    /// `objectref` must be of type `reference` or `returnAddress`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Astore {
        /// The index into the local variable array.
        index: u8,
    },
    /// Equivalent to `istore 0`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Istore0,
    /// Equivalent to `istore 1`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Istore1,
    /// Equivalent to `istore 2`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Istore2,
    /// Equivalent to `istore 3`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Istore3,
    /// Equivalent to `lstore 0`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Lstore0,
    /// Equivalent to `lstore 1`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Lstore1,
    /// Equivalent to `lstore 2`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Lstore2,
    /// Equivalent to `lstore 3`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Lstore3,
    /// Equivalent to `fstore 0`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Fstore0,
    /// Equivalent to `fstore 1`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Fstore1,
    /// Equivalent to `fstore 2`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Fstore2,
    /// Equivalent to `fstore 3`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Fstore3,
    /// Equivalent to `dstore 0`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Dstore0,
    /// Equivalent to `dstore 1`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Dstore1,
    /// Equivalent to `dstore 2`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Dstore2,
    /// Equivalent to `dstore 3`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Dstore3,
    /// Equivalent to `astore 0`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Astore0,
    /// Equivalent to `astore 1`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Astore1,
    /// Equivalent to `astore 2`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Astore2,
    /// Equivalent to `astore 3`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Astore3,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `int`. `index` must be an `int`. `value` must be a `int`.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Iastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `long`. `index` must be an `int`. `value` must be a `long`.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Lastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `float`. `index` must be an `int`. `value` must be a `float`.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Fastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `double`. `index` must be an `int`. `value` must be a `double`.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Dastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `reference`. `index` must be an `int`. `value` must be a
    /// `reference`.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Aastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is either byte or boolean. `index` and `value` must be `int`s.
    /// `value` is truncated before being stored.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Bastore,
    /// Sets the value at `arrayref[index]` to `value`. `arrayref` must be a `reference` to an
    /// array whose element type is `char`. `index` and `value` must be `int`s. `value` is
    /// truncated before being stored.
    /// # Operand stack:
    /// ..., arrayref, index, value ->
    ///
    /// ... ->
    Castore,
    /// Pops the top 4-byte value from the operand stack. `value` must be a 4-byte value.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Pop,
    /// Pops the top two 4-byte values or the top 8-byte value from the operand stack. `value` must
    /// be an 8-byte value. `value1` and `value2` must both be 4-byte values.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    ///
    /// *or*
    ///
    /// ..., value2, value1 ->
    ///
    /// ... ->
    Pop2,
    /// Duplicates the top 4-byte value on the stack. The type of `value` must be exactly 4 bytes
    /// in size. As of the Java Virtual Machine Specification for Java SE 7, that includes all
    /// types except `long` and `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., value, value ->
    Dup,
    /// Duplicates the top 4-byte value on the stack and inserts the copy at the position third
    /// from the top. Both `value1` and `value2` must be exactly 4 bytes in size. As of the Java
    /// Virtual Machine Specification for Java SE 7, that includes all types except `long` and
    /// `double`.
    /// # Operand stack:
    /// ..., value2, value1 ->
    ///
    /// ..., value1, value2, value1 ->
    DupX1,
    /// Duplicates the top 4-byte value on the stack and inserts the copy at the position fourth
    /// from the top. `value1` must be exactly 4 bytes in size. `value2` and `value3` must both be
    /// exactly 4 bytes in size but may form a single 8-byte value. As of the Java Virtual Machine
    /// Specification for Java SE 7, all types are 4 bytes in size except `long` and `double`,
    /// which are both 8 bytes.
    /// # Operand stack:
    /// ..., value3, value2, value1 ->
    ///
    /// ..., value1, value3, value2, value1 ->
    DupX2,
    /// Duplicates the top two 4-byte values on the stack and inserts the copies in the same order
    /// at the positions third and fourth from the top. `value1` and `value2` must both be exactly
    /// 4 bytes in size, but may form a single 8-byte value. As of the Java Virtual Machine
    /// Specification for Java SE 7, all types are 4 bytes in size except `long` and `double`,
    /// which are both 8 bytes.
    /// # Operand stack:
    /// ..., value2, value1 ->
    ///
    /// ..., value2, value1, value2, value1 ->
    Dup2,
    /// Duplicates the top two 4-byte values on the stack and inserts the copies in the same order
    /// at the positions fourth and fifth from the top. `value1` and `value2` must both be exactly
    /// 4 bytes in size, but may form a single 8-byte value. `value3` must be exactly 4 bytes in
    /// size. As of the Java Virtual Machine Specification for Java SE 7, all types are 4 bytes in
    /// size except `long` and `double`, which are both 8 bytes.
    /// # Operand stack:
    /// ..., value3, value2, value1 ->
    ///
    /// ..., value2, value1, value3, value2, value1 ->
    Dup2X1,
    /// Duplicates the top two 4-byte values on the stack and inserts the copies in the same order
    /// at the positions fifth and sixth from the top. `value1` and `value2` must both be exactly 4
    /// bytes in size, but may form a single 8-byte value. `value3` and `value4` must both be
    /// exactly 4 bytes in size, but may form a single 8-byte value. As of the Java Virtual Machine
    /// Specification for Java SE 7, all types are 4 bytes in size except `long` and `double`,
    /// which are both 8 bytes.
    /// # Operand stack:
    /// ..., value4, value3, value2, value1 ->
    ///
    /// ..., value2, value1, value4, value3, value2, value1 ->
    Dup2X2,
    /// Computes `value1 + value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Iadd,
    /// Computes `value1 + value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ladd,
    /// Computes `value1 + value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fadd,
    /// Computes `value1 + value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Dadd,
    /// Computes `value1 - value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Isub,
    /// Computes `value1 - value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lsub,
    /// Computes `value1 - value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fsub,
    /// Computes `value1 - value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Dsub,
    /// Computes `value1 * value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Imul,
    /// Computes `value1 * value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lmul,
    /// Computes `value1 * value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fmul,
    /// Computes `value1 * value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Dmul,
    /// Computes `value1 / value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Idiv,
    /// Computes `value1 / value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ldiv,
    /// Computes `value1 / value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fdiv,
    /// Computes `value1 / value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ddiv,
    /// Computes `value1 - value2 * (value1 / value2)` and pushes the result onto the stack.
    /// `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Irem,
    /// Computes `value1 - value2 * (value1 / value2)` and pushes the result onto the stack.
    /// `value1` and `value2` must both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lrem,
    /// Computes `value1 - value2 * q` and pushes the result onto the stack where
    /// `sign(q) == sign(value1 / value2)` and `abs(q) == floor(abs(value1 / value2))`. `value1` and
    /// `value2` must both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Frem,
    /// Computes `value1 - value2 * q` and pushes the result onto the stack where `sign(q) == -1`
    /// only if `sign(value1 / value2) == -1`, `sign(q) == 1` only if `sign(value1 / value2)`, and
    /// `abs(q) == floor(abs(value1 / value2))`. `value1` and `value2` must both be of type
    /// `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Drem,
    /// Computes `-value` and pushes the result onto the stack. `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    Ineg,
    /// Computes `-value` and pushes the result onto the stack. `value` must be of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    Lneg,
    /// Computes `-value` and pushes the result onto the stack. `value` must be of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    Fneg,
    /// Computes `-value` and pushes the result onto the stack. `value` must be of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    Dneg,
    /// Compute `value1 << (value2 & 0x1F)` and push the result onto the stack. `value1` and
    /// `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ishl,
    /// Compute `value1 << (value2 & 0x3F)` and push the result onto the stack. `value1` must be of
    /// type 'long'. `value2` must be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lshl,
    /// Compute `value1 >> (value2 & 0x1F)` and push the result onto the stack. `value1` and
    /// `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ishr,
    /// Compute `value1 >> (value2 & 0x3F)` and push the result onto the stack. `value1` must be of
    /// type `long`. `value2` must be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lshr,
    /// Compute `value1 >>> (value2 & 0x1F)` and push the result onto the stack. `value1` and
    /// `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Iushr,
    /// Compute `value1 >>> (value2 & 0x3F)` and push the result onto the stack. `value1` must be
    /// of type `long`. `value2` must be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lushr,
    /// Computes `value1 & value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Iand,
    /// Computes `value1 & value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Land,
    /// Computes `value1 | value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ior,
    /// Computes `value1 | value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lor,
    /// Computes `value1 ^ value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Ixor,
    /// Computes `value1 ^ value2` and pushes the result onto the stack. `value1` and `value2` must
    /// both be of type `long`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lxor,
    /// Increment the local variable at index `index` in the local variable array by `delta`. The
    /// local variable at index `index` must be of type `int`. `delta` is sign-extended before
    /// being added to the local variable.
    /// # Operand stack:
    /// ... ->
    ///
    /// ... ->
    Iinc {
        /// The index into the local variable array.
        index: u8,
        /// The value to add to the local variable.
        delta: i8,
    },
    /// Converts a `int` value to a `long` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2l,
    /// Converts a `int` value to a `float` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2f,
    /// Converts a `int` value to a `double` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2d,
    /// Converts a `long` value to an `int` and pushes the result onto the stack. `value` must be
    /// of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    L2i,
    /// Converts a `long` value to a `float` and pushes the result onto the stack. `value` must be
    /// of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    L2f,
    /// Converts a `long` value to a `double` and pushes the result onto the stack. `value` must be
    /// of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    L2d,
    /// Converts a `float` value to an `int` and pushes the result onto the stack. `value` must be
    /// of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    F2i,
    /// Converts a `float` value to a `long` and pushes the result onto the stack. `value` must be
    /// of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// .., result ->
    F2l,
    /// Converts a `float` value to a `double` and pushes the result onto the stack. `value` must
    /// be of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    F2d,
    /// Converts a `double` value to an `int` and pushes the result onto the stack. `value` must be
    /// of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    D2i,
    /// Converts a `double` value to a `long` and pushes the result onto the stack. `value` must be
    /// of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    D2l,
    /// Convert a `double` value to a `float` and pushes the result onto the stack. `value` must be
    /// of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    D2f,
    /// Convert a `int` value to a `byte` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2b,
    /// Convert a `int` value to a `char` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2c,
    /// Convert a `int` value to a `short` and pushes the result onto the stack. `value` must be
    /// of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ..., result ->
    I2s,
    /// Compares `value1` and `value2` and pushes the result onto the stack. The result of the
    /// comparison is the `int` with magnitude 0 or 1 and the same sign as `value1 - value2`.
    /// `value1` and `value2` must both be of type `long`.
    /// # Operand stack
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Lcmp,
    /// Compares `value1` and `value2` and pushes the result onto the stack. If `value1` is NaN or
    /// `value2` is NaN, then the result is -1. If `value1` is less than `value2`, then the result
    /// is -1. If `value1` is equal to `value2`, then the result is 0. If `value1` is greater than
    /// `value2`, then the result is 1. `value1` and `value2` must both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fcmpl,
    /// Compares `value1` and `value2` and pushes the result onto the stack. If `value1` is NaN or
    /// `value2` is NaN, then the result is 1. If `value1` is less than `value2`, then the result
    /// is -1. If `value1` is equal to `value2`, then the result is 0. If `value1` is greater than
    /// `value2`, then the result is 1. `value1` and `value2` must both be of type `float`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Fcmpg,
    /// Compares `value1` and `value2` and pushes the result onto the stack. If `value1` is NaN or
    /// `value2` is NaN, then the result is -1. If `value1` is less than `value2`, then the result
    /// is -1. If `value1` is equal to `value2`, then the result is 0. If `value1` is greater than
    /// `value2`, then the result is 1. `value1` and `value2` must both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Dcmpl,
    /// Compares `value1` and `value2` and pushes the result onto the stack. If `value1` is NaN or
    /// `value2` is NaN, then the result is 1. If `value1` is less than `value2`, then the result
    /// is -1. If `value1` is equal to `value2`, then the result is 0. If `value1` is greater than
    /// `value2`, then the result is 1. `value1` and `value2` must both be of type `double`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ..., result ->
    Dcmpg,
    /// Jump to the specified offset relative to the address of this instruction if `value == 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifeq {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value != 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifne {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value < 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Iflt {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value >= 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifge {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value > 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifgt {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value <= 0`.
    /// `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifle {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 ==
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmpeq {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 !=
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmpne {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 <
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmplt {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 >=
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmpge {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 >
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmpgt {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 <=
    /// value2`. `value1` and `value2` must both be of type `int`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfIcmple {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 ==
    /// value2`. `value1` and `value2` must both be of type `reference`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfAcmpeq {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value1 !=
    /// value2`. `value1` and `value2` must both be of type `reference`.
    /// # Operand stack:
    /// ..., value1, value2 ->
    ///
    /// ... ->
    IfAcmpne {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Unconditionally jump to the specified offset relative to the address of this instruction.
    /// # Operand stack:
    /// ... ->
    ///
    /// ... ->
    Goto {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Push the address of the next instruction onto the operand stack then jump to the specified
    /// offset relative to the address of this instruction. The target instruction must be within
    /// the current function.
    ///
    /// This instruction has historically been used in Oracle's implementation of the Java compiler
    /// for the `finally` clause in versions of the compiler before Java SE 6.
    ///
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., address ->
    Jsr {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Access jump table and jump. If there is some `(match, offset)` pair in `match_offsets` such
    /// that `match == key`, jump to `offset`. Otherwise, jump to `default_offset`. The elements `e`
    /// of `match_offsets` must be such that `e.0` is monotonically increasing.
    /// # Operand stack:
    /// ..., key ->
    ///
    /// ... ->
    Lookupswitch {
        /// The offset of the `default` label for the switch statement.
        default_offset: i32,
        /// The `(key, offset)` pairs of the non-`default` labels for the switch statement.
        match_offsets: Vec<(i32, i32)>,
    },
    /// Return an `int` from a function. `value` must be of type `int`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// <no stack>
    Ireturn,
    /// Return a `long` from a function. `value` must be of type `long`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// <no stack>
    Lreturn,
    /// Return a `float` from a function. `value` must be of type `float`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// <no stack>
    Freturn,
    /// Return a `double` from a function. `value` must be of type `double`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// <no stack>
    Dreturn,
    /// Return a `reference` from a function.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// <no stack>
    Areturn,
    /// Read the specified static field and push the result onto the stack.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., value ->
    Getstatic {
        /// A symbolic reference to the static field to read.
        field: FieldRef,
    },
    /// Read the specified (non-static) field from `objectref` and push the result onto the stack.
    /// `objectref` must be a `reference` to a type which is a subclass of `field.owner()`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ..., value ->
    Getfield {
        /// A symbolic reference to the instance field to read.
        field: FieldRef,
    },
    /// Write to the specified (non-static) field from `objectref`.
    /// `objectref` must be a `reference` to a type which is a subclass of `field.owner()`.
    /// If `field`'s nominal type is a primitive integral type other than `long`, then `value` must
    /// be of type `int`. If `field`'s nominal type is any other primitive type, then `value` must
    /// be of the same type. If `field`'s nominal type is some reference type, then `value` must be
    /// a `reference` to an instance of that type. If `field` is `final`, the current method must
    /// be `<init>` of `field.owner()`.
    /// # Operand stack:
    /// ..., objectref, value ->
    ///
    /// ... ->
    Putfield {
        /// A symbolic reference to the instance field to write.
        field: FieldRef,
    },
    /// Invoke instance method on `objectref`. `objectref` must be a `reference` to a type which is
    /// a subtype of `method.owner()`. Invokespecial differs from Invokevirtual in the treatment of
    /// references to methods owned by a superclass of the runtime class of `objectref`. In
    /// particular, Invokespecial starts at the owner of the method reference whereas Invokevirtual
    /// starts at the runtime class of `objectref`.
    /// # Operand stack:
    /// ..., objectref, [arg1, [arg2, [...]]] ->
    ///
    /// ... ->
    Invokevirtual {
        /// A symbolic reference to the instance method to invoke.
        method: MethodRef,
    },
    /// Invoke instance method on `objectref`. `objectref` must be a `reference` to a type which is
    /// a subtype of `method.owner()`. Invokespecial differs from Invokevirtual in the treatment of
    /// references to methods owned by a superclass of the runtime class of `objectref`. In
    /// particular, Invokespecial starts at the owner of the method reference whereas Invokevirtual
    /// starts at the runtime class of `objectref`.
    /// # Operand stack:
    /// ..., objectref, [arg1, [arg2, [...]]] ->
    ///
    /// ... ->
    Invokespecial {
        /// A symbolic reference to the instance method to invoke.
        method: MethodRef,
    },
    /// Invoke static method.
    /// # Operand stack:
    /// ..., [arg1, [arg2, [...]]] ->
    ///
    /// ... ->
    Invokestatic {
        /// A symbolic reference to the static method to invoke.
        method: MethodRef,
    },
    /// TODO: comprehend description
    /// # Operand stack:
    /// ..., [arg1, [arg2, [...]]] ->
    ///
    /// ... ->
    Invokedynamic {
        /// The index into the `bootstrap_methods` list of the `BootstrapMethods` attribute of the
        /// current class of the method to call.
        bootstrap_idx: u16,
        /// The name of the method to call.
        name: JavaIdentifier,
        /// The type of the method to call.
        r#type: JavaType,
    },
    /// Create a new object of the class type represented by the symbolic reference at index
    /// `index` in the runtime constant pool. All instance variables of the class are initialized
    /// to their default values.
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., objectref ->
    New {
        /// The type to instantiate.
        type_name: QualifiedClassName,
    },
    /// Create a new array with element type `type` of length `count`. Initialize each element of
    /// the created array to the default value.
    /// # Operand stack:
    /// ..., count ->
    ///
    /// ..., arrayref ->
    Newarray {
        /// The element type of the array to create.
        r#type: PrimitiveValueType,
    },
    /// Create a new array whose element type is `reference` and length is `count`. `count` must be
    /// an `int`. `el_type` must be a qualified class name (non-primitive type) from the constant
    /// pool.
    /// # Operand stack:
    /// ..., count ->
    ///
    /// ..., arrayref ->
    Anewarray {
        /// The element type of the array to create.
        el_type: QualifiedClassName,
    },
    /// Compute `arrayref.length`, where `arrayref` is a `reference` to an array.
    /// # Operand stack:
    /// ..., arrayref ->
    ///
    /// ..., length ->
    Arraylength,
    /// Throw the `Throwable` referred to by `objectref`. `objectref` must be a `reference` to a
    /// `Throwable`. If no exception handler in the current stack frame can handle the object
    /// referred to by `objectref`, then the current stack frame is popped and `objectref` is
    /// rethrown.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// objectref ->
    Athrow,
    /// Asserts that `objectref` is either `null` or an instance of `type_name`. If the assertion
    /// fails, a `ClassCastException` is thrown.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ..., objectref ->
    Checkcast {
        /// The type to assert that `objectref` is an instance of.
        type_name: QualifiedClassName,
    },
    /// Tests that `objectref` is an instance of `type_name`. `objectref` must be of type
    /// `reference`. If `objectref` is a reference to an instance of a subtype of `type_name`, then
    /// `result` is 1, otherwise `result` is 0.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ..., result ->
    Instanceof {
        /// The type to check that `objectref` is an instance of.
        type_name: QualifiedClassName,
    },
    /// Acquire the (re-entrant) lock associated with `objectref`. `objectref` must be of type
    /// `reference`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Monitorenter,
    /// Release the (re-entrant) lock associated with `objectref`. `objectref` must be of type
    /// `reference`.
    /// # Operand stack:
    /// ..., objectref ->
    ///
    /// ... ->
    Monitorexit,
    /// Create a new array with at least `dimensions` dimensions. The constant pool entry at index
    /// `index` must be a symbolic reference to an array type with at least `dimensions`
    /// dimensions. Create a new array with that type and length `count1`. Each subarray for which
    /// there is an associated `count` is initialized to be of length `count`. The last subarray
    /// for which there is an associated `count` has all of its elements initialized to the default
    /// value of its element type.
    /// # Operand stack:
    /// ..., count1, [count2, ...] ->
    ///
    /// ..., arrayref ->
    Multianewarray {
        /// The actual type of the array to create. Must have at least `dimensions` dimensions.
        /// TODO: convert to owned type.
        index: i16,
        /// The number of dimensions to initialize. The `i`th dimension is initialized to be arrays
        /// of length `counti`. The `dimensions + 1`st dimension is initialized to be the default
        /// value for its type.
        dimensions: u8,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value ==
    /// null`. `value` must be of type `reference`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifnull {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the address of this instruction if `value !=
    /// null`. `value` must be of type `reference`.
    /// # Operand stack:
    /// ..., value ->
    ///
    /// ... ->
    Ifnonnull {
        /// The offset relative to the address of this instruction to jump to.
        offset: i16,
    },
    /// Jump to the specified offset relative to the current address.
    /// # Operand stack:
    /// ... ->
    ///
    /// ... ->
    GotoW {
        /// The offset relative to the address of this instruction to jump to. Due to current
        /// constraints on method size, `offset.abs()` should never be too large to fit into a
        /// `u16`.
        offset: i32,
    },
    /// Push the address of the next instruction onto the operand stack then jump to the specified
    /// offset relative to the address of this instruction. The target instruction must be within
    /// the current function.
    ///
    /// This instruction has historically been used in Oracle's implementation of the Java compiler
    /// for the `finally` clause in versions of the compiler before Java SE 6.
    ///
    /// # Operand stack:
    /// ... ->
    ///
    /// ..., address ->
    JsrW {
        /// The offset relative to the address of this instruction to jump to. Due to current
        /// constraints on method size, `offset.abs()` should never be too large to fit into a
        /// `u16`.
        offset: i32,
    },
}

impl JavaOpCode {
    const NOP: u8 = 0x00;
    const ACONST_NULL: u8 = 0x01;
    const ICONST_M1: u8 = 0x02;
    const ICONST_0: u8 = 0x03;
    const ICONST_1: u8 = 0x04;
    const ICONST_2: u8 = 0x05;
    const ICONST_3: u8 = 0x06;
    const ICONST_4: u8 = 0x07;
    const ICONST_5: u8 = 0x08;
    const LCONST_0: u8 = 0x09;
    const LCONST_1: u8 = 0x0A;
    const FCONST_0: u8 = 0x0B;
    const FCONST_1: u8 = 0x0C;
    const FCONST_2: u8 = 0x0D;
    const DCONST_0: u8 = 0x0E;
    const DCONST_1: u8 = 0x0F;

    const BIPUSH: u8 = 0x10;
    const _11: u8 = 0x11;
    const LDC: u8 = 0x12;
    const LDC_W: u8 = 0x13;
    const LDC2_W: u8 = 0x14;
    const ILOAD: u8 = 0x15;
    const LLOAD: u8 = 0x16;
    const FLOAD: u8 = 0x17;
    const DLOAD: u8 = 0x18;
    const ALOAD: u8 = 0x19;
    const ILOAD_0: u8 = 0x1A;
    const ILOAD_1: u8 = 0x1B;
    const ILOAD_2: u8 = 0x1C;
    const ILOAD_3: u8 = 0x1D;
    const LLOAD_0: u8 = 0x1E;
    const LLOAD_1: u8 = 0x1F;

    const LLOAD_2: u8 = 0x20;
    const LLOAD_3: u8 = 0x21;
    const FLOAD_0: u8 = 0x22;
    const FLOAD_1: u8 = 0x23;
    const FLOAD_2: u8 = 0x24;
    const FLOAD_3: u8 = 0x25;
    const DLOAD_0: u8 = 0x26;
    const DLOAD_1: u8 = 0x27;
    const DLOAD_2: u8 = 0x28;
    const DLOAD_3: u8 = 0x29;
    const ALOAD_0: u8 = 0x2A;
    const ALOAD_1: u8 = 0x2B;
    const ALOAD_2: u8 = 0x2C;
    const ALOAD_3: u8 = 0x2D;
    const IALOAD: u8 = 0x2E;
    const LALOAD: u8 = 0x2F;

    const FALOAD: u8 = 0x30;
    const DALOAD: u8 = 0x31;
    const AALOAD: u8 = 0x32;
    const BALOAD: u8 = 0x33;
    const CALOAD: u8 = 0x34;
    const _35: u8 = 0x35;
    const ISTORE: u8 = 0x36;
    const LSTORE: u8 = 0x37;
    const FSTORE: u8 = 0x38;
    const DSTORE: u8 = 0x39;
    const ASTORE: u8 = 0x3A;
    const ISTORE_0: u8 = 0x3B;
    const ISTORE_1: u8 = 0x3C;
    const ISTORE_2: u8 = 0x3D;
    const ISTORE_3: u8 = 0x3E;
    const LSTORE_0: u8 = 0x3F;

    const LSTORE_1: u8 = 0x40;
    const LSTORE_2: u8 = 0x41;
    const LSTORE_3: u8 = 0x42;
    const FSTORE_0: u8 = 0x43;
    const FSTORE_1: u8 = 0x44;
    const FSTORE_2: u8 = 0x45;
    const FSTORE_3: u8 = 0x46;
    const DSTORE_0: u8 = 0x47;
    const DSTORE_1: u8 = 0x48;
    const DSTORE_2: u8 = 0x49;
    const DSTORE_3: u8 = 0x4A;
    const ASTORE_0: u8 = 0x4B;
    const ASTORE_1: u8 = 0x4C;
    const ASTORE_2: u8 = 0x4D;
    const ASTORE_3: u8 = 0x4E;
    const IASTORE: u8 = 0x4F;

    const LASTORE: u8 = 0x50;
    const FASTORE: u8 = 0x51;
    const DASTORE: u8 = 0x52;
    const AASTORE: u8 = 0x53;
    const BASTORE: u8 = 0x54;
    const CASTORE: u8 = 0x55;
    const _56: u8 = 0x56;
    const POP: u8 = 0x57;
    const POP2: u8 = 0x58;
    const DUP: u8 = 0x59;
    const DUP_X1: u8 = 0x5A;
    const DUP_X2: u8 = 0x5B;
    const DUP2: u8 = 0x5C;
    const DUP2_X1: u8 = 0x5D;
    const DUP2_X2: u8 = 0x5E;
    const _5F: u8 = 0x5F;

    const IADD: u8 = 0x60;
    const LADD: u8 = 0x61;
    const FADD: u8 = 0x62;
    const DADD: u8 = 0x63;
    const ISUB: u8 = 0x64;
    const LSUB: u8 = 0x65;
    const FSUB: u8 = 0x66;
    const DSUB: u8 = 0x67;
    const IMUL: u8 = 0x68;
    const LMUL: u8 = 0x69;
    const FMUL: u8 = 0x6A;
    const DMUL: u8 = 0x6B;
    const IDIV: u8 = 0x6C;
    const LDIV: u8 = 0x6D;
    const FDIV: u8 = 0x6E;
    const DDIV: u8 = 0x6F;

    const IREM: u8 = 0x70;
    const LREM: u8 = 0x71;
    const FREM: u8 = 0x72;
    const DREM: u8 = 0x73;
    const INEG: u8 = 0x74;
    const LNEG: u8 = 0x75;
    const FNEG: u8 = 0x76;
    const DNEG: u8 = 0x77;
    const ISHL: u8 = 0x78;
    const LSHL: u8 = 0x79;
    const ISHR: u8 = 0x7A;
    const LSHR: u8 = 0x7B;
    const IUSHR: u8 = 0x7C;
    const LUSHR: u8 = 0x7D;
    const IAND: u8 = 0x7E;
    const LAND: u8 = 0x7F;

    const IOR: u8 = 0x80;
    const LOR: u8 = 0x81;
    const IXOR: u8 = 0x82;
    const LXOR: u8 = 0x83;
    const IINC: u8 = 0x84;
    const I2L: u8 = 0x85;
    const I2F: u8 = 0x86;
    const I2D: u8 = 0x87;
    const L2I: u8 = 0x88;
    const L2F: u8 = 0x89;
    const L2D: u8 = 0x8A;
    const F2I: u8 = 0x8B;
    const F2L: u8 = 0x8C;
    const F2D: u8 = 0x8D;
    const D2I: u8 = 0x8E;
    const D2L: u8 = 0x8F;

    const D2F: u8 = 0x90;
    const I2B: u8 = 0x91;
    const I2C: u8 = 0x92;
    const I2S: u8 = 0x93;
    const LCMP: u8 = 0x94;
    const FCMPL: u8 = 0x95;
    const FCMPG: u8 = 0x96;
    const DCMPL: u8 = 0x97;
    const DCMPG: u8 = 0x98;
    const IFEQ: u8 = 0x99;
    const IFNE: u8 = 0x9A;
    const IFLT: u8 = 0x9B;
    const IFGE: u8 = 0x9C;
    const IFGT: u8 = 0x9D;
    const IFLE: u8 = 0x9E;
    const IF_ICMPEQ: u8 = 0x9F;

    const IF_ICMPNE: u8 = 0xA0;
    const IF_ICMPLT: u8 = 0xA1;
    const IF_ICMPGE: u8 = 0xA2;
    const IF_ICMPGT: u8 = 0xA3;
    const IF_ICMPLE: u8 = 0xA4;
    const IF_ACMPEQ: u8 = 0xA5;
    const IF_ACMPNE: u8 = 0xA6;
    const GOTO: u8 = 0xA7;
    const JSR: u8 = 0xA8;
    const _A9: u8 = 0xA9;
    const _AA: u8 = 0xAA;
    const LOOKUPSWITCH: u8 = 0xAB;
    const IRETURN: u8 = 0xAC;
    const LRETURN: u8 = 0xAD;
    const FRETURN: u8 = 0xAE;
    const DRETURN: u8 = 0xAF;

    const ARETURN: u8 = 0xB0;
    const _B1: u8 = 0xB1;
    const GETSTATIC: u8 = 0xB2;
    const _B3: u8 = 0xB3;
    const GETFIELD: u8 = 0xB4;
    const PUTFIELD: u8 = 0xB5;
    const INVOKEVIRTUAL: u8 = 0xB6;
    const INVOKESPECIAL: u8 = 0xB7;
    const INVOKESTATIC: u8 = 0xB8;
    const _B9: u8 = 0xB9;
    const INVOKEDYNAMIC: u8 = 0xBA;
    const NEW: u8 = 0xBB;
    const NEWARRAY: u8 = 0xBC;
    const ANEWARRAY: u8 = 0xBD;
    const ARRAYLENGTH: u8 = 0xBE;
    const ATHROW: u8 = 0xBF;

    const CHECKCAST: u8 = 0xC0;
    const INSTANCEOF: u8 = 0xC1;
    const MONITORENTER: u8 = 0xC2;
    const MONITOREXIT: u8 = 0xC3;
    const _C4: u8 = 0xC4;
    const MULTIANEWARRAY: u8 = 0xC5;
    const IFNULL: u8 = 0xC6;
    const IFNONNULL: u8 = 0xC7;
    const GOTO_W: u8 = 0xC8;
    const JSR_W: u8 = 0xC9;
    const _CA: u8 = 0xCA;
    const _CB: u8 = 0xCB;
    const _CC: u8 = 0xCC;
    const _CD: u8 = 0xCD;
    const _CE: u8 = 0xCE;
    const _CF: u8 = 0xCF;

    const _D0: u8 = 0xD0;
    const _D1: u8 = 0xD1;
    const _D2: u8 = 0xD2;
    const _D3: u8 = 0xD3;
    const _D4: u8 = 0xD4;
    const _D5: u8 = 0xD5;
    const _D6: u8 = 0xD6;
    const _D7: u8 = 0xD7;
    const _D8: u8 = 0xD8;
    const _D9: u8 = 0xD9;
    const _DA: u8 = 0xDA;
    const _DB: u8 = 0xDB;
    const _DC: u8 = 0xDC;
    const _DD: u8 = 0xDD;
    const _DE: u8 = 0xDE;
    const _DF: u8 = 0xDF;

    const _E0: u8 = 0xE0;
    const _E1: u8 = 0xE1;
    const _E2: u8 = 0xE2;
    const _E3: u8 = 0xE3;
    const _E4: u8 = 0xE4;
    const _E5: u8 = 0xE5;
    const _E6: u8 = 0xE6;
    const _E7: u8 = 0xE7;
    const _E8: u8 = 0xE8;
    const _E9: u8 = 0xE9;
    const _EA: u8 = 0xEA;
    const _EB: u8 = 0xEB;
    const _EC: u8 = 0xEC;
    const _ED: u8 = 0xED;
    const _EE: u8 = 0xEE;
    const _EF: u8 = 0xEF;

    const _F0: u8 = 0xF0;
    const _F1: u8 = 0xF1;
    const _F2: u8 = 0xF2;
    const _F3: u8 = 0xF3;
    const _F4: u8 = 0xF4;
    const _F5: u8 = 0xF5;
    const _F6: u8 = 0xF6;
    const _F7: u8 = 0xF7;
    const _F8: u8 = 0xF8;
    const _F9: u8 = 0xF9;
    const _FA: u8 = 0xFA;
    const _FB: u8 = 0xFB;
    const _FC: u8 = 0xFC;
    const _FD: u8 = 0xFD;
    const _FE: u8 = 0xFE;
    const _FF: u8 = 0xFF;

    fn len(&self, position: u16) -> u16 {
        match self {
            JavaOpCode::Nop
            | JavaOpCode::AconstNull
            | JavaOpCode::IconstM1
            | JavaOpCode::Iconst0
            | JavaOpCode::Iconst1
            | JavaOpCode::Iconst2
            | JavaOpCode::Iconst3
            | JavaOpCode::Iconst4
            | JavaOpCode::Iconst5
            | JavaOpCode::Lconst0
            | JavaOpCode::Lconst1
            | JavaOpCode::Fconst0
            | JavaOpCode::Fconst1
            | JavaOpCode::Fconst2
            | JavaOpCode::Dconst0
            | JavaOpCode::Dconst1
            | JavaOpCode::Iload0
            | JavaOpCode::Iload1
            | JavaOpCode::Iload2
            | JavaOpCode::Iload3
            | JavaOpCode::Lload0
            | JavaOpCode::Lload1
            | JavaOpCode::Lload2
            | JavaOpCode::Lload3
            | JavaOpCode::Fload0
            | JavaOpCode::Fload1
            | JavaOpCode::Fload2
            | JavaOpCode::Fload3
            | JavaOpCode::Dload0
            | JavaOpCode::Dload1
            | JavaOpCode::Dload2
            | JavaOpCode::Dload3
            | JavaOpCode::Aload0
            | JavaOpCode::Aload1
            | JavaOpCode::Aload2
            | JavaOpCode::Aload3
            | JavaOpCode::Iaload
            | JavaOpCode::Laload
            | JavaOpCode::Faload
            | JavaOpCode::Daload
            | JavaOpCode::Aaload
            | JavaOpCode::Baload
            | JavaOpCode::Caload
            | JavaOpCode::Istore0
            | JavaOpCode::Istore1
            | JavaOpCode::Istore2
            | JavaOpCode::Istore3
            | JavaOpCode::Lstore0
            | JavaOpCode::Lstore1
            | JavaOpCode::Lstore2
            | JavaOpCode::Lstore3
            | JavaOpCode::Fstore0
            | JavaOpCode::Fstore1
            | JavaOpCode::Fstore2
            | JavaOpCode::Fstore3
            | JavaOpCode::Dstore0
            | JavaOpCode::Dstore1
            | JavaOpCode::Dstore2
            | JavaOpCode::Dstore3
            | JavaOpCode::Astore0
            | JavaOpCode::Astore1
            | JavaOpCode::Astore2
            | JavaOpCode::Astore3
            | JavaOpCode::Iastore
            | JavaOpCode::Lastore
            | JavaOpCode::Fastore
            | JavaOpCode::Dastore
            | JavaOpCode::Aastore
            | JavaOpCode::Bastore
            | JavaOpCode::Castore
            | JavaOpCode::Pop
            | JavaOpCode::Pop2
            | JavaOpCode::Dup
            | JavaOpCode::DupX1
            | JavaOpCode::DupX2
            | JavaOpCode::Dup2
            | JavaOpCode::Dup2X1
            | JavaOpCode::Dup2X2
            | JavaOpCode::Iadd
            | JavaOpCode::Ladd
            | JavaOpCode::Fadd
            | JavaOpCode::Dadd
            | JavaOpCode::Isub
            | JavaOpCode::Lsub
            | JavaOpCode::Fsub
            | JavaOpCode::Dsub
            | JavaOpCode::Imul
            | JavaOpCode::Lmul
            | JavaOpCode::Fmul
            | JavaOpCode::Dmul
            | JavaOpCode::Idiv
            | JavaOpCode::Ldiv
            | JavaOpCode::Fdiv
            | JavaOpCode::Ddiv
            | JavaOpCode::Irem
            | JavaOpCode::Lrem
            | JavaOpCode::Frem
            | JavaOpCode::Drem
            | JavaOpCode::Ineg
            | JavaOpCode::Lneg
            | JavaOpCode::Fneg
            | JavaOpCode::Dneg
            | JavaOpCode::Ishl
            | JavaOpCode::Lshl
            | JavaOpCode::Ishr
            | JavaOpCode::Lshr
            | JavaOpCode::Iushr
            | JavaOpCode::Lushr
            | JavaOpCode::Iand
            | JavaOpCode::Land
            | JavaOpCode::Ior
            | JavaOpCode::Lor
            | JavaOpCode::Ixor
            | JavaOpCode::Lxor
            | JavaOpCode::I2l
            | JavaOpCode::I2f
            | JavaOpCode::I2d
            | JavaOpCode::L2i
            | JavaOpCode::L2f
            | JavaOpCode::L2d
            | JavaOpCode::F2i
            | JavaOpCode::F2l
            | JavaOpCode::F2d
            | JavaOpCode::D2i
            | JavaOpCode::D2l
            | JavaOpCode::D2f
            | JavaOpCode::I2b
            | JavaOpCode::I2c
            | JavaOpCode::I2s
            | JavaOpCode::Lcmp
            | JavaOpCode::Fcmpl
            | JavaOpCode::Fcmpg
            | JavaOpCode::Dcmpl
            | JavaOpCode::Dcmpg
            | JavaOpCode::Ireturn
            | JavaOpCode::Lreturn
            | JavaOpCode::Freturn
            | JavaOpCode::Dreturn
            | JavaOpCode::Areturn
            | JavaOpCode::Arraylength
            | JavaOpCode::Athrow
            | JavaOpCode::Monitorenter
            | JavaOpCode::Monitorexit => 1,
            JavaOpCode::Bipush { .. }
            | JavaOpCode::Ldc { .. }
            | JavaOpCode::Iload { .. }
            | JavaOpCode::Lload { .. }
            | JavaOpCode::Fload { .. }
            | JavaOpCode::Dload { .. }
            | JavaOpCode::Aload { .. }
            | JavaOpCode::Istore { .. }
            | JavaOpCode::Lstore { .. }
            | JavaOpCode::Fstore { .. }
            | JavaOpCode::Dstore { .. }
            | JavaOpCode::Astore { .. }
            | JavaOpCode::Newarray { .. } => 2,
            JavaOpCode::LdcW { .. }
            | JavaOpCode::Ldc2W { .. }
            | JavaOpCode::Iinc { .. }
            | JavaOpCode::Ifeq { .. }
            | JavaOpCode::Ifne { .. }
            | JavaOpCode::Iflt { .. }
            | JavaOpCode::Ifge { .. }
            | JavaOpCode::Ifgt { .. }
            | JavaOpCode::Ifle { .. }
            | JavaOpCode::IfIcmpeq { .. }
            | JavaOpCode::IfIcmpne { .. }
            | JavaOpCode::IfIcmplt { .. }
            | JavaOpCode::IfIcmpge { .. }
            | JavaOpCode::IfIcmpgt { .. }
            | JavaOpCode::IfIcmple { .. }
            | JavaOpCode::IfAcmpeq { .. }
            | JavaOpCode::IfAcmpne { .. }
            | JavaOpCode::Goto { .. }
            | JavaOpCode::Jsr { .. }
            | JavaOpCode::Getstatic { .. }
            | JavaOpCode::Getfield { .. }
            | JavaOpCode::Putfield { .. }
            | JavaOpCode::Invokevirtual { .. }
            | JavaOpCode::Invokespecial { .. }
            | JavaOpCode::Invokestatic { .. }
            | JavaOpCode::New { .. }
            | JavaOpCode::Anewarray { .. }
            | JavaOpCode::Checkcast { .. }
            | JavaOpCode::Instanceof { .. }
            | JavaOpCode::Ifnull { .. }
            | JavaOpCode::Ifnonnull { .. } => 3,
            JavaOpCode::Multianewarray { .. } => 4,
            JavaOpCode::Invokedynamic { .. }
            | JavaOpCode::GotoW { .. }
            | JavaOpCode::JsrW { .. } => 5,
            JavaOpCode::Lookupswitch {
                ref match_offsets, ..
            } => 1 + ((position + 1) % 4) + 4 + 4 + 8 * match_offsets.len() as u16,
        }
    }

    fn into_raw(self, idx: u16, pool: &mut ConstantPool) -> CPAccessResult<Vec<u8>> {
        let mut ret = vec![];
        match self {
            JavaOpCode::Nop => ret.push(JavaOpCode::NOP),
            JavaOpCode::AconstNull => ret.push(JavaOpCode::ACONST_NULL),
            JavaOpCode::IconstM1 => ret.push(JavaOpCode::ICONST_M1),
            JavaOpCode::Iconst0 => ret.push(JavaOpCode::ICONST_0),
            JavaOpCode::Iconst1 => ret.push(JavaOpCode::ICONST_1),
            JavaOpCode::Iconst2 => ret.push(JavaOpCode::ICONST_2),
            JavaOpCode::Iconst3 => ret.push(JavaOpCode::ICONST_3),
            JavaOpCode::Iconst4 => ret.push(JavaOpCode::ICONST_4),
            JavaOpCode::Iconst5 => ret.push(JavaOpCode::ICONST_5),
            JavaOpCode::Lconst0 => ret.push(JavaOpCode::LCONST_0),
            JavaOpCode::Lconst1 => ret.push(JavaOpCode::LCONST_1),
            JavaOpCode::Fconst0 => ret.push(JavaOpCode::FCONST_0),
            JavaOpCode::Fconst1 => ret.push(JavaOpCode::FCONST_1),
            JavaOpCode::Fconst2 => ret.push(JavaOpCode::FCONST_2),
            JavaOpCode::Dconst0 => ret.push(JavaOpCode::DCONST_0),
            JavaOpCode::Dconst1 => ret.push(JavaOpCode::DCONST_1),
            JavaOpCode::Bipush { value } => {
                ret.push(JavaOpCode::BIPUSH);
                ret.push(value);
            }
            JavaOpCode::Ldc { index } => {
                ret.push(JavaOpCode::LDC);
                ret.push(index);
            }
            JavaOpCode::LdcW { index } => {
                ret.push(JavaOpCode::LDC_W);
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Ldc2W { value } => {
                ret.push(JavaOpCode::LDC2_W);
                let index = value
                    .map_left(|l| pool.add_long(l))
                    .map_right(|d| pool.add_double(d))
                    .unwrap()?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Iload { index } => match index {
                0 => return JavaOpCode::Iload0.into_raw(idx, pool),
                1 => return JavaOpCode::Iload1.into_raw(idx, pool),
                2 => return JavaOpCode::Iload2.into_raw(idx, pool),
                3 => return JavaOpCode::Iload3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::ILOAD);
                    ret.push(index);
                }
            },
            JavaOpCode::Lload { index } => match index {
                0 => return JavaOpCode::Lload0.into_raw(idx, pool),
                1 => return JavaOpCode::Lload1.into_raw(idx, pool),
                2 => return JavaOpCode::Lload2.into_raw(idx, pool),
                3 => return JavaOpCode::Lload3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::LLOAD);
                    ret.push(index);
                }
            },
            JavaOpCode::Fload { index } => match index {
                0 => return JavaOpCode::Fload0.into_raw(idx, pool),
                1 => return JavaOpCode::Fload1.into_raw(idx, pool),
                2 => return JavaOpCode::Fload2.into_raw(idx, pool),
                3 => return JavaOpCode::Fload3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::FLOAD);
                    ret.push(index);
                }
            },
            JavaOpCode::Dload { index } => match index {
                0 => return JavaOpCode::Dload0.into_raw(idx, pool),
                1 => return JavaOpCode::Dload1.into_raw(idx, pool),
                2 => return JavaOpCode::Dload2.into_raw(idx, pool),
                3 => return JavaOpCode::Dload3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::DLOAD);
                    ret.push(index);
                }
            },
            JavaOpCode::Aload { index } => match index {
                0 => return JavaOpCode::Aload0.into_raw(idx, pool),
                1 => return JavaOpCode::Aload1.into_raw(idx, pool),
                2 => return JavaOpCode::Aload2.into_raw(idx, pool),
                3 => return JavaOpCode::Aload3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::ALOAD);
                    ret.push(index);
                }
            },
            JavaOpCode::Iload0 => ret.push(JavaOpCode::ILOAD_0),
            JavaOpCode::Iload1 => ret.push(JavaOpCode::ILOAD_1),
            JavaOpCode::Iload2 => ret.push(JavaOpCode::ILOAD_2),
            JavaOpCode::Iload3 => ret.push(JavaOpCode::ILOAD_3),
            JavaOpCode::Lload0 => ret.push(JavaOpCode::LLOAD_0),
            JavaOpCode::Lload1 => ret.push(JavaOpCode::LLOAD_1),
            JavaOpCode::Lload2 => ret.push(JavaOpCode::LLOAD_2),
            JavaOpCode::Lload3 => ret.push(JavaOpCode::LLOAD_3),
            JavaOpCode::Fload0 => ret.push(JavaOpCode::FLOAD_0),
            JavaOpCode::Fload1 => ret.push(JavaOpCode::FLOAD_1),
            JavaOpCode::Fload2 => ret.push(JavaOpCode::FLOAD_2),
            JavaOpCode::Fload3 => ret.push(JavaOpCode::FLOAD_3),
            JavaOpCode::Dload0 => ret.push(JavaOpCode::DLOAD_0),
            JavaOpCode::Dload1 => ret.push(JavaOpCode::DLOAD_1),
            JavaOpCode::Dload2 => ret.push(JavaOpCode::DLOAD_2),
            JavaOpCode::Dload3 => ret.push(JavaOpCode::DLOAD_3),
            JavaOpCode::Aload0 => ret.push(JavaOpCode::ALOAD_0),
            JavaOpCode::Aload1 => ret.push(JavaOpCode::ALOAD_1),
            JavaOpCode::Aload2 => ret.push(JavaOpCode::ALOAD_2),
            JavaOpCode::Aload3 => ret.push(JavaOpCode::ALOAD_3),
            JavaOpCode::Iaload => ret.push(JavaOpCode::IALOAD),
            JavaOpCode::Laload => ret.push(JavaOpCode::LALOAD),
            JavaOpCode::Faload => ret.push(JavaOpCode::FALOAD),
            JavaOpCode::Daload => ret.push(JavaOpCode::DALOAD),
            JavaOpCode::Aaload => ret.push(JavaOpCode::AALOAD),
            JavaOpCode::Baload => ret.push(JavaOpCode::BALOAD),
            JavaOpCode::Caload => ret.push(JavaOpCode::CALOAD),
            JavaOpCode::Istore { index } => match index {
                0 => return JavaOpCode::Istore0.into_raw(idx, pool),
                1 => return JavaOpCode::Istore1.into_raw(idx, pool),
                2 => return JavaOpCode::Istore2.into_raw(idx, pool),
                3 => return JavaOpCode::Istore3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::ISTORE);
                    ret.push(index);
                }
            },
            JavaOpCode::Lstore { index } => match index {
                0 => return JavaOpCode::Lstore0.into_raw(idx, pool),
                1 => return JavaOpCode::Lstore1.into_raw(idx, pool),
                2 => return JavaOpCode::Lstore2.into_raw(idx, pool),
                3 => return JavaOpCode::Lstore3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::LSTORE);
                    ret.push(index);
                }
            },
            JavaOpCode::Fstore { index } => match index {
                0 => return JavaOpCode::Fstore0.into_raw(idx, pool),
                1 => return JavaOpCode::Fstore1.into_raw(idx, pool),
                2 => return JavaOpCode::Fstore2.into_raw(idx, pool),
                3 => return JavaOpCode::Fstore3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::FSTORE);
                    ret.push(index);
                }
            },
            JavaOpCode::Dstore { index } => match index {
                0 => return JavaOpCode::Dstore0.into_raw(idx, pool),
                1 => return JavaOpCode::Dstore1.into_raw(idx, pool),
                2 => return JavaOpCode::Dstore2.into_raw(idx, pool),
                3 => return JavaOpCode::Dstore3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::DSTORE);
                    ret.push(index);
                }
            },
            JavaOpCode::Astore { index } => match index {
                0 => return JavaOpCode::Astore0.into_raw(idx, pool),
                1 => return JavaOpCode::Astore1.into_raw(idx, pool),
                2 => return JavaOpCode::Astore2.into_raw(idx, pool),
                3 => return JavaOpCode::Astore3.into_raw(idx, pool),
                _ => {
                    ret.push(JavaOpCode::ASTORE);
                    ret.push(index);
                }
            },
            JavaOpCode::Istore0 => ret.push(JavaOpCode::ISTORE_0),
            JavaOpCode::Istore1 => ret.push(JavaOpCode::ISTORE_1),
            JavaOpCode::Istore2 => ret.push(JavaOpCode::ISTORE_2),
            JavaOpCode::Istore3 => ret.push(JavaOpCode::ISTORE_3),
            JavaOpCode::Lstore0 => ret.push(JavaOpCode::ISTORE_0),
            JavaOpCode::Lstore1 => ret.push(JavaOpCode::ISTORE_1),
            JavaOpCode::Lstore2 => ret.push(JavaOpCode::ISTORE_2),
            JavaOpCode::Lstore3 => ret.push(JavaOpCode::ISTORE_3),
            JavaOpCode::Fstore0 => ret.push(JavaOpCode::FSTORE_0),
            JavaOpCode::Fstore1 => ret.push(JavaOpCode::FSTORE_1),
            JavaOpCode::Fstore2 => ret.push(JavaOpCode::FSTORE_2),
            JavaOpCode::Fstore3 => ret.push(JavaOpCode::FSTORE_3),
            JavaOpCode::Dstore0 => ret.push(JavaOpCode::DSTORE_0),
            JavaOpCode::Dstore1 => ret.push(JavaOpCode::DSTORE_1),
            JavaOpCode::Dstore2 => ret.push(JavaOpCode::DSTORE_2),
            JavaOpCode::Dstore3 => ret.push(JavaOpCode::DSTORE_3),
            JavaOpCode::Astore0 => ret.push(JavaOpCode::ASTORE_0),
            JavaOpCode::Astore1 => ret.push(JavaOpCode::ASTORE_1),
            JavaOpCode::Astore2 => ret.push(JavaOpCode::ASTORE_2),
            JavaOpCode::Astore3 => ret.push(JavaOpCode::ASTORE_3),
            JavaOpCode::Iastore => ret.push(JavaOpCode::IASTORE),
            JavaOpCode::Lastore => ret.push(JavaOpCode::LASTORE),
            JavaOpCode::Fastore => ret.push(JavaOpCode::FASTORE),
            JavaOpCode::Dastore => ret.push(JavaOpCode::DASTORE),
            JavaOpCode::Aastore => ret.push(JavaOpCode::AASTORE),
            JavaOpCode::Bastore => ret.push(JavaOpCode::BASTORE),
            JavaOpCode::Castore => ret.push(JavaOpCode::CASTORE),
            JavaOpCode::Pop => ret.push(JavaOpCode::POP),
            JavaOpCode::Pop2 => ret.push(JavaOpCode::POP2),
            JavaOpCode::Dup => ret.push(JavaOpCode::DUP),
            JavaOpCode::DupX1 => ret.push(JavaOpCode::DUP_X1),
            JavaOpCode::DupX2 => ret.push(JavaOpCode::DUP_X2),
            JavaOpCode::Dup2 => ret.push(JavaOpCode::DUP2),
            JavaOpCode::Dup2X1 => ret.push(JavaOpCode::DUP2_X1),
            JavaOpCode::Dup2X2 => ret.push(JavaOpCode::DUP2_X2),
            JavaOpCode::Iadd => ret.push(JavaOpCode::IADD),
            JavaOpCode::Ladd => ret.push(JavaOpCode::LADD),
            JavaOpCode::Fadd => ret.push(JavaOpCode::FADD),
            JavaOpCode::Dadd => ret.push(JavaOpCode::DADD),
            JavaOpCode::Isub => ret.push(JavaOpCode::ISUB),
            JavaOpCode::Lsub => ret.push(JavaOpCode::LSUB),
            JavaOpCode::Fsub => ret.push(JavaOpCode::FSUB),
            JavaOpCode::Dsub => ret.push(JavaOpCode::DSUB),
            JavaOpCode::Imul => ret.push(JavaOpCode::IMUL),
            JavaOpCode::Lmul => ret.push(JavaOpCode::LMUL),
            JavaOpCode::Fmul => ret.push(JavaOpCode::FMUL),
            JavaOpCode::Dmul => ret.push(JavaOpCode::DMUL),
            JavaOpCode::Idiv => ret.push(JavaOpCode::IDIV),
            JavaOpCode::Ldiv => ret.push(JavaOpCode::LDIV),
            JavaOpCode::Fdiv => ret.push(JavaOpCode::FDIV),
            JavaOpCode::Ddiv => ret.push(JavaOpCode::DDIV),
            JavaOpCode::Irem => ret.push(JavaOpCode::IREM),
            JavaOpCode::Lrem => ret.push(JavaOpCode::LREM),
            JavaOpCode::Frem => ret.push(JavaOpCode::FREM),
            JavaOpCode::Drem => ret.push(JavaOpCode::DREM),
            JavaOpCode::Ineg => ret.push(JavaOpCode::INEG),
            JavaOpCode::Lneg => ret.push(JavaOpCode::LNEG),
            JavaOpCode::Fneg => ret.push(JavaOpCode::FNEG),
            JavaOpCode::Dneg => ret.push(JavaOpCode::DNEG),
            JavaOpCode::Ishl => ret.push(JavaOpCode::ISHL),
            JavaOpCode::Lshl => ret.push(JavaOpCode::LSHL),
            JavaOpCode::Ishr => ret.push(JavaOpCode::ISHR),
            JavaOpCode::Lshr => ret.push(JavaOpCode::LSHR),
            JavaOpCode::Iushr => ret.push(JavaOpCode::IUSHR),
            JavaOpCode::Lushr => ret.push(JavaOpCode::LUSHR),
            JavaOpCode::Iand => ret.push(JavaOpCode::IAND),
            JavaOpCode::Land => ret.push(JavaOpCode::LAND),
            JavaOpCode::Ior => ret.push(JavaOpCode::IOR),
            JavaOpCode::Lor => ret.push(JavaOpCode::LOR),
            JavaOpCode::Ixor => ret.push(JavaOpCode::IXOR),
            JavaOpCode::Lxor => ret.push(JavaOpCode::LXOR),
            JavaOpCode::Iinc { index, delta } => {
                ret.push(JavaOpCode::IINC);
                ret.push(index);
                ret.extend(&delta.to_be_bytes());
            }
            JavaOpCode::I2l => ret.push(JavaOpCode::I2L),
            JavaOpCode::I2f => ret.push(JavaOpCode::I2F),
            JavaOpCode::I2d => ret.push(JavaOpCode::I2D),
            JavaOpCode::L2i => ret.push(JavaOpCode::L2I),
            JavaOpCode::L2f => ret.push(JavaOpCode::L2F),
            JavaOpCode::L2d => ret.push(JavaOpCode::L2D),
            JavaOpCode::F2i => ret.push(JavaOpCode::F2I),
            JavaOpCode::F2l => ret.push(JavaOpCode::F2L),
            JavaOpCode::F2d => ret.push(JavaOpCode::F2D),
            JavaOpCode::D2i => ret.push(JavaOpCode::D2I),
            JavaOpCode::D2l => ret.push(JavaOpCode::D2L),
            JavaOpCode::D2f => ret.push(JavaOpCode::D2F),
            JavaOpCode::I2b => ret.push(JavaOpCode::I2B),
            JavaOpCode::I2c => ret.push(JavaOpCode::I2C),
            JavaOpCode::I2s => ret.push(JavaOpCode::I2S),
            JavaOpCode::Lcmp => ret.push(JavaOpCode::LCMP),
            JavaOpCode::Fcmpl => ret.push(JavaOpCode::FCMPL),
            JavaOpCode::Fcmpg => ret.push(JavaOpCode::FCMPG),
            JavaOpCode::Dcmpl => ret.push(JavaOpCode::DCMPL),
            JavaOpCode::Dcmpg => ret.push(JavaOpCode::DCMPG),
            JavaOpCode::Ifeq { offset } => {
                ret.push(JavaOpCode::IFEQ);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Ifne { offset } => {
                ret.push(JavaOpCode::IFNE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Iflt { offset } => {
                ret.push(JavaOpCode::IFLT);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Ifge { offset } => {
                ret.push(JavaOpCode::IFGE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Ifgt { offset } => {
                ret.push(JavaOpCode::IFGT);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Ifle { offset } => {
                ret.push(JavaOpCode::IFLE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmpeq { offset } => {
                ret.push(JavaOpCode::IF_ICMPEQ);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmpne { offset } => {
                ret.push(JavaOpCode::IF_ICMPNE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmplt { offset } => {
                ret.push(JavaOpCode::IF_ICMPLT);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmpge { offset } => {
                ret.push(JavaOpCode::IF_ICMPGE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmpgt { offset } => {
                ret.push(JavaOpCode::IF_ICMPGT);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfIcmple { offset } => {
                ret.push(JavaOpCode::IF_ICMPLE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfAcmpeq { offset } => {
                ret.push(JavaOpCode::IF_ACMPEQ);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::IfAcmpne { offset } => {
                ret.push(JavaOpCode::IF_ACMPNE);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Goto { offset } => {
                ret.push(JavaOpCode::GOTO);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Jsr { offset } => {
                ret.push(JavaOpCode::JSR);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Lookupswitch {
                default_offset,
                match_offsets,
            } => {
                ret.push(JavaOpCode::LOOKUPSWITCH);
                ret.extend(std::iter::repeat(0).take(((idx + 1) % 4).into()));
                ret.extend(&default_offset.to_be_bytes());
                ret.extend(&(match_offsets.len() as u32).to_be_bytes());
                for (r#match, offset) in match_offsets {
                    ret.extend(&r#match.to_be_bytes());
                    ret.extend(&offset.to_be_bytes());
                }
            }
            JavaOpCode::Ireturn => ret.push(JavaOpCode::IRETURN),
            JavaOpCode::Lreturn => ret.push(JavaOpCode::LRETURN),
            JavaOpCode::Freturn => ret.push(JavaOpCode::FRETURN),
            JavaOpCode::Dreturn => ret.push(JavaOpCode::DRETURN),
            JavaOpCode::Areturn => ret.push(JavaOpCode::ARETURN),
            JavaOpCode::Getstatic { field } => {
                ret.push(JavaOpCode::GETSTATIC);
                let index = pool.add_field_ref(field)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Getfield { field } => {
                ret.push(JavaOpCode::GETFIELD);
                let index = pool.add_field_ref(field)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Putfield { field } => {
                ret.push(JavaOpCode::PUTFIELD);
                let index = pool.add_field_ref(field)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Invokevirtual { method } => {
                ret.push(JavaOpCode::INVOKEVIRTUAL);
                let index = pool.add_method_ref(method)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Invokespecial { method } => {
                ret.push(JavaOpCode::INVOKESPECIAL);
                let index = pool.add_method_ref(method)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Invokestatic { method } => {
                ret.push(JavaOpCode::INVOKESTATIC);
                let index = pool.add_method_ref(method)?;
                ret.extend(&index.to_be_bytes());
            }
            JavaOpCode::Invokedynamic {
                bootstrap_idx,
                name,
                r#type,
            } => {
                ret.push(JavaOpCode::INVOKEDYNAMIC);
                let nat_idx = pool.add_name_and_type(name, r#type)?;
                let id_idx = pool.add(CPEntry::InvokeDynamic(bootstrap_idx, nat_idx))?;
                ret.extend(&id_idx.to_be_bytes());
                ret.extend(&[0, 0]);
            }
            JavaOpCode::New { type_name } => {
                ret.push(JavaOpCode::NEW);
                ret.extend(&pool.add_class_name(type_name)?.to_be_bytes());
            }
            JavaOpCode::Newarray { r#type } => {
                ret.push(JavaOpCode::NEWARRAY);
                ret.push(r#type as u8);
            }
            JavaOpCode::Anewarray { el_type } => {
                ret.push(JavaOpCode::ANEWARRAY);
                let type_index = pool.add_class_name(el_type)?;
                ret.extend(&type_index.to_be_bytes());
            }
            JavaOpCode::Arraylength => ret.push(JavaOpCode::ARRAYLENGTH),
            JavaOpCode::Athrow => ret.push(JavaOpCode::ATHROW),
            JavaOpCode::Checkcast { type_name } => {
                ret.push(JavaOpCode::CHECKCAST);
                let type_index = pool.add_class_name(type_name)?;
                ret.extend(&type_index.to_be_bytes());
            }
            JavaOpCode::Instanceof { type_name } => {
                ret.push(JavaOpCode::INSTANCEOF);
                let type_index = pool.add_class_name(type_name)?;
                ret.extend(&type_index.to_be_bytes());
            }
            JavaOpCode::Monitorenter => ret.push(JavaOpCode::MONITORENTER),
            JavaOpCode::Monitorexit => ret.push(JavaOpCode::MONITOREXIT),
            JavaOpCode::Multianewarray { index, dimensions } => {
                ret.push(JavaOpCode::MULTIANEWARRAY);
                ret.extend(&index.to_be_bytes());
                ret.push(dimensions);
            }
            JavaOpCode::Ifnull { offset } => {
                ret.push(JavaOpCode::IFNULL);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::Ifnonnull { offset } => {
                ret.push(JavaOpCode::IFNONNULL);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::GotoW { offset } => {
                ret.push(JavaOpCode::GOTO_W);
                ret.extend(&offset.to_be_bytes());
            }
            JavaOpCode::JsrW { offset } => {
                ret.push(JavaOpCode::JSR_W);
                ret.extend(&offset.to_be_bytes());
            }
        }
        Ok(ret)
    }
}

impl<'i, 'pool> NomParse<(u16, &'pool ConstantPool), &'i [u8]> for JavaOpCode {
    type Output = CrateResult<Self>;

    fn nom_parse((idx, pool): Self::Env, s: Self::Input) -> IResult<Self::Input, Self::Output> {
        comb::flat_map(num::be_u8, |opcode| {
            let ret: Box<dyn Parser<_, _, _>> = match opcode {
                opcode if opcode == JavaOpCode::NOP => Box::new(just!(Ok(JavaOpCode::Nop))),
                opcode if opcode == JavaOpCode::ACONST_NULL => {
                    Box::new(just!(Ok(JavaOpCode::AconstNull)))
                }
                opcode if opcode == JavaOpCode::ICONST_M1 => {
                    Box::new(just!(Ok(JavaOpCode::IconstM1)))
                }
                opcode if opcode == JavaOpCode::ICONST_0 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst0)))
                }
                opcode if opcode == JavaOpCode::ICONST_1 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst1)))
                }
                opcode if opcode == JavaOpCode::ICONST_2 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst2)))
                }
                opcode if opcode == JavaOpCode::ICONST_3 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst3)))
                }
                opcode if opcode == JavaOpCode::ICONST_4 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst4)))
                }
                opcode if opcode == JavaOpCode::ICONST_5 => {
                    Box::new(just!(Ok(JavaOpCode::Iconst5)))
                }
                opcode if opcode == JavaOpCode::LCONST_0 => {
                    Box::new(just!(Ok(JavaOpCode::Lconst0)))
                }
                opcode if opcode == JavaOpCode::LCONST_1 => {
                    Box::new(just!(Ok(JavaOpCode::Lconst1)))
                }
                opcode if opcode == JavaOpCode::FCONST_0 => {
                    Box::new(just!(Ok(JavaOpCode::Fconst0)))
                }
                opcode if opcode == JavaOpCode::FCONST_1 => {
                    Box::new(just!(Ok(JavaOpCode::Fconst1)))
                }
                opcode if opcode == JavaOpCode::FCONST_2 => {
                    Box::new(just!(Ok(JavaOpCode::Fconst2)))
                }
                opcode if opcode == JavaOpCode::DCONST_0 => {
                    Box::new(just!(Ok(JavaOpCode::Dconst0)))
                }
                opcode if opcode == JavaOpCode::DCONST_1 => {
                    Box::new(just!(Ok(JavaOpCode::Dconst1)))
                }
                opcode if opcode == JavaOpCode::BIPUSH => {
                    Box::new(comb::map(num::be_u8, |value| {
                        Ok(JavaOpCode::Bipush { value })
                    }))
                }
                opcode if opcode == JavaOpCode::LDC => {
                    Box::new(comb::map(num::be_u8, |index| Ok(JavaOpCode::Ldc { index })))
                }
                opcode if opcode == JavaOpCode::LDC_W => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::LdcW { index })
                    }))
                }
                opcode if opcode == JavaOpCode::LDC2_W => {
                    Box::new(comb::map(num::be_u16, |index| {
                        let entry = pool.get(index)?;
                        let value = match entry {
                            CPEntry::Double(d) => Either::Right(*d),
                            CPEntry::Long(l) => Either::Left(*l),
                            _ => {
                                return Err(format!(
                                    "Ldc2W must refer to a double or a long. Refers to {}",
                                    entry.r#type(),
                                )
                                .into())
                            }
                        };
                        Ok(JavaOpCode::Ldc2W { value })
                    }))
                }
                opcode if opcode == JavaOpCode::ILOAD => Box::new(comb::map(num::be_u8, |index| {
                    Ok(JavaOpCode::Iload { index })
                })),
                opcode if opcode == JavaOpCode::LLOAD => Box::new(comb::map(num::be_u8, |index| {
                    Ok(JavaOpCode::Lload { index })
                })),
                opcode if opcode == JavaOpCode::FLOAD => Box::new(comb::map(num::be_u8, |index| {
                    Ok(JavaOpCode::Fload { index })
                })),
                opcode if opcode == JavaOpCode::DLOAD => Box::new(comb::map(num::be_u8, |index| {
                    Ok(JavaOpCode::Dload { index })
                })),
                opcode if opcode == JavaOpCode::ALOAD => Box::new(comb::map(num::be_u8, |index| {
                    Ok(JavaOpCode::Aload { index })
                })),
                opcode if opcode == JavaOpCode::ILOAD_0 => Box::new(just!(Ok(JavaOpCode::Iload0))),
                opcode if opcode == JavaOpCode::ILOAD_1 => Box::new(just!(Ok(JavaOpCode::Iload1))),
                opcode if opcode == JavaOpCode::ILOAD_2 => Box::new(just!(Ok(JavaOpCode::Iload2))),
                opcode if opcode == JavaOpCode::ILOAD_3 => Box::new(just!(Ok(JavaOpCode::Iload3))),
                opcode if opcode == JavaOpCode::LLOAD_0 => Box::new(just!(Ok(JavaOpCode::Lload0))),
                opcode if opcode == JavaOpCode::LLOAD_1 => Box::new(just!(Ok(JavaOpCode::Lload1))),
                opcode if opcode == JavaOpCode::LLOAD_2 => Box::new(just!(Ok(JavaOpCode::Lload2))),
                opcode if opcode == JavaOpCode::LLOAD_3 => Box::new(just!(Ok(JavaOpCode::Lload3))),
                opcode if opcode == JavaOpCode::FLOAD_0 => Box::new(just!(Ok(JavaOpCode::Fload0))),
                opcode if opcode == JavaOpCode::FLOAD_1 => Box::new(just!(Ok(JavaOpCode::Fload1))),
                opcode if opcode == JavaOpCode::FLOAD_2 => Box::new(just!(Ok(JavaOpCode::Fload2))),
                opcode if opcode == JavaOpCode::FLOAD_3 => Box::new(just!(Ok(JavaOpCode::Fload3))),
                opcode if opcode == JavaOpCode::DLOAD_0 => Box::new(just!(Ok(JavaOpCode::Dload0))),
                opcode if opcode == JavaOpCode::DLOAD_1 => Box::new(just!(Ok(JavaOpCode::Dload1))),
                opcode if opcode == JavaOpCode::DLOAD_2 => Box::new(just!(Ok(JavaOpCode::Dload2))),
                opcode if opcode == JavaOpCode::DLOAD_3 => Box::new(just!(Ok(JavaOpCode::Dload3))),
                opcode if opcode == JavaOpCode::ALOAD_0 => Box::new(just!(Ok(JavaOpCode::Aload0))),
                opcode if opcode == JavaOpCode::ALOAD_1 => Box::new(just!(Ok(JavaOpCode::Aload1))),
                opcode if opcode == JavaOpCode::ALOAD_2 => Box::new(just!(Ok(JavaOpCode::Aload2))),
                opcode if opcode == JavaOpCode::ALOAD_3 => Box::new(just!(Ok(JavaOpCode::Aload3))),
                opcode if opcode == JavaOpCode::IALOAD => Box::new(just!(Ok(JavaOpCode::Iaload))),
                opcode if opcode == JavaOpCode::LALOAD => Box::new(just!(Ok(JavaOpCode::Laload))),
                opcode if opcode == JavaOpCode::FALOAD => Box::new(just!(Ok(JavaOpCode::Faload))),
                opcode if opcode == JavaOpCode::DALOAD => Box::new(just!(Ok(JavaOpCode::Daload))),
                opcode if opcode == JavaOpCode::AALOAD => Box::new(just!(Ok(JavaOpCode::Aaload))),
                opcode if opcode == JavaOpCode::BALOAD => Box::new(just!(Ok(JavaOpCode::Baload))),
                opcode if opcode == JavaOpCode::CALOAD => Box::new(just!(Ok(JavaOpCode::Caload))),
                opcode if opcode == JavaOpCode::ISTORE => {
                    Box::new(comb::map(num::be_u8, |index| {
                        Ok(JavaOpCode::Istore { index })
                    }))
                }
                opcode if opcode == JavaOpCode::LSTORE => {
                    Box::new(comb::map(num::be_u8, |index| {
                        Ok(JavaOpCode::Lstore { index })
                    }))
                }
                opcode if opcode == JavaOpCode::FSTORE => {
                    Box::new(comb::map(num::be_u8, |index| {
                        Ok(JavaOpCode::Fstore { index })
                    }))
                }
                opcode if opcode == JavaOpCode::DSTORE => {
                    Box::new(comb::map(num::be_u8, |index| {
                        Ok(JavaOpCode::Dstore { index })
                    }))
                }
                opcode if opcode == JavaOpCode::ASTORE => {
                    Box::new(comb::map(num::be_u8, |index| {
                        Ok(JavaOpCode::Astore { index })
                    }))
                }
                opcode if opcode == JavaOpCode::ISTORE_0 => {
                    Box::new(just!(Ok(JavaOpCode::Istore0)))
                }
                opcode if opcode == JavaOpCode::ISTORE_1 => {
                    Box::new(just!(Ok(JavaOpCode::Istore1)))
                }
                opcode if opcode == JavaOpCode::ISTORE_2 => {
                    Box::new(just!(Ok(JavaOpCode::Istore2)))
                }
                opcode if opcode == JavaOpCode::ISTORE_3 => {
                    Box::new(just!(Ok(JavaOpCode::Istore3)))
                }
                opcode if opcode == JavaOpCode::LSTORE_0 => {
                    Box::new(just!(Ok(JavaOpCode::Lstore0)))
                }
                opcode if opcode == JavaOpCode::LSTORE_1 => {
                    Box::new(just!(Ok(JavaOpCode::Lstore1)))
                }
                opcode if opcode == JavaOpCode::LSTORE_2 => {
                    Box::new(just!(Ok(JavaOpCode::Lstore2)))
                }
                opcode if opcode == JavaOpCode::LSTORE_3 => {
                    Box::new(just!(Ok(JavaOpCode::Lstore3)))
                }
                opcode if opcode == JavaOpCode::FSTORE_0 => {
                    Box::new(just!(Ok(JavaOpCode::Fstore0)))
                }
                opcode if opcode == JavaOpCode::FSTORE_1 => {
                    Box::new(just!(Ok(JavaOpCode::Fstore1)))
                }
                opcode if opcode == JavaOpCode::FSTORE_2 => {
                    Box::new(just!(Ok(JavaOpCode::Fstore2)))
                }
                opcode if opcode == JavaOpCode::FSTORE_3 => {
                    Box::new(just!(Ok(JavaOpCode::Fstore3)))
                }
                opcode if opcode == JavaOpCode::DSTORE_0 => {
                    Box::new(just!(Ok(JavaOpCode::Dstore0)))
                }
                opcode if opcode == JavaOpCode::DSTORE_1 => {
                    Box::new(just!(Ok(JavaOpCode::Dstore1)))
                }
                opcode if opcode == JavaOpCode::DSTORE_2 => {
                    Box::new(just!(Ok(JavaOpCode::Dstore2)))
                }
                opcode if opcode == JavaOpCode::DSTORE_3 => {
                    Box::new(just!(Ok(JavaOpCode::Dstore3)))
                }
                opcode if opcode == JavaOpCode::ASTORE_0 => {
                    Box::new(just!(Ok(JavaOpCode::Astore0)))
                }
                opcode if opcode == JavaOpCode::ASTORE_1 => {
                    Box::new(just!(Ok(JavaOpCode::Astore1)))
                }
                opcode if opcode == JavaOpCode::ASTORE_2 => {
                    Box::new(just!(Ok(JavaOpCode::Astore2)))
                }
                opcode if opcode == JavaOpCode::ASTORE_3 => {
                    Box::new(just!(Ok(JavaOpCode::Astore3)))
                }
                opcode if opcode == JavaOpCode::IASTORE => Box::new(just!(Ok(JavaOpCode::Iastore))),
                opcode if opcode == JavaOpCode::LASTORE => Box::new(just!(Ok(JavaOpCode::Lastore))),
                opcode if opcode == JavaOpCode::FASTORE => Box::new(just!(Ok(JavaOpCode::Fastore))),
                opcode if opcode == JavaOpCode::DASTORE => Box::new(just!(Ok(JavaOpCode::Dastore))),
                opcode if opcode == JavaOpCode::AASTORE => Box::new(just!(Ok(JavaOpCode::Aastore))),
                opcode if opcode == JavaOpCode::BASTORE => Box::new(just!(Ok(JavaOpCode::Bastore))),
                opcode if opcode == JavaOpCode::CASTORE => Box::new(just!(Ok(JavaOpCode::Castore))),
                opcode if opcode == JavaOpCode::POP => Box::new(just!(Ok(JavaOpCode::Pop))),
                opcode if opcode == JavaOpCode::POP2 => Box::new(just!(Ok(JavaOpCode::Pop2))),
                opcode if opcode == JavaOpCode::DUP => Box::new(just!(Ok(JavaOpCode::Dup))),
                opcode if opcode == JavaOpCode::DUP_X1 => Box::new(just!(Ok(JavaOpCode::DupX1))),
                opcode if opcode == JavaOpCode::DUP_X2 => Box::new(just!(Ok(JavaOpCode::DupX2))),
                opcode if opcode == JavaOpCode::DUP2 => Box::new(just!(Ok(JavaOpCode::Dup2))),
                opcode if opcode == JavaOpCode::DUP2_X1 => Box::new(just!(Ok(JavaOpCode::Dup2X1))),
                opcode if opcode == JavaOpCode::DUP2_X2 => Box::new(just!(Ok(JavaOpCode::Dup2X2))),
                opcode if opcode == JavaOpCode::IADD => Box::new(just!(Ok(JavaOpCode::Iadd))),
                opcode if opcode == JavaOpCode::LADD => Box::new(just!(Ok(JavaOpCode::Ladd))),
                opcode if opcode == JavaOpCode::FADD => Box::new(just!(Ok(JavaOpCode::Fadd))),
                opcode if opcode == JavaOpCode::DADD => Box::new(just!(Ok(JavaOpCode::Dadd))),
                opcode if opcode == JavaOpCode::ISUB => Box::new(just!(Ok(JavaOpCode::Isub))),
                opcode if opcode == JavaOpCode::LSUB => Box::new(just!(Ok(JavaOpCode::Lsub))),
                opcode if opcode == JavaOpCode::FSUB => Box::new(just!(Ok(JavaOpCode::Fsub))),
                opcode if opcode == JavaOpCode::DSUB => Box::new(just!(Ok(JavaOpCode::Dsub))),
                opcode if opcode == JavaOpCode::IMUL => Box::new(just!(Ok(JavaOpCode::Imul))),
                opcode if opcode == JavaOpCode::LMUL => Box::new(just!(Ok(JavaOpCode::Lmul))),
                opcode if opcode == JavaOpCode::FMUL => Box::new(just!(Ok(JavaOpCode::Fmul))),
                opcode if opcode == JavaOpCode::DMUL => Box::new(just!(Ok(JavaOpCode::Dmul))),
                opcode if opcode == JavaOpCode::IDIV => Box::new(just!(Ok(JavaOpCode::Idiv))),
                opcode if opcode == JavaOpCode::LDIV => Box::new(just!(Ok(JavaOpCode::Ldiv))),
                opcode if opcode == JavaOpCode::FDIV => Box::new(just!(Ok(JavaOpCode::Fdiv))),
                opcode if opcode == JavaOpCode::DDIV => Box::new(just!(Ok(JavaOpCode::Ddiv))),
                opcode if opcode == JavaOpCode::IREM => Box::new(just!(Ok(JavaOpCode::Irem))),
                opcode if opcode == JavaOpCode::LREM => Box::new(just!(Ok(JavaOpCode::Lrem))),
                opcode if opcode == JavaOpCode::FREM => Box::new(just!(Ok(JavaOpCode::Frem))),
                opcode if opcode == JavaOpCode::DREM => Box::new(just!(Ok(JavaOpCode::Drem))),
                opcode if opcode == JavaOpCode::INEG => Box::new(just!(Ok(JavaOpCode::Ineg))),
                opcode if opcode == JavaOpCode::LNEG => Box::new(just!(Ok(JavaOpCode::Lneg))),
                opcode if opcode == JavaOpCode::FNEG => Box::new(just!(Ok(JavaOpCode::Fneg))),
                opcode if opcode == JavaOpCode::DNEG => Box::new(just!(Ok(JavaOpCode::Dneg))),
                opcode if opcode == JavaOpCode::ISHL => Box::new(just!(Ok(JavaOpCode::Ishl))),
                opcode if opcode == JavaOpCode::LSHL => Box::new(just!(Ok(JavaOpCode::Lshl))),
                opcode if opcode == JavaOpCode::ISHR => Box::new(just!(Ok(JavaOpCode::Ishr))),
                opcode if opcode == JavaOpCode::LSHR => Box::new(just!(Ok(JavaOpCode::Lshr))),
                opcode if opcode == JavaOpCode::IUSHR => Box::new(just!(Ok(JavaOpCode::Iushr))),
                opcode if opcode == JavaOpCode::LUSHR => Box::new(just!(Ok(JavaOpCode::Lushr))),
                opcode if opcode == JavaOpCode::IAND => Box::new(just!(Ok(JavaOpCode::Iand))),
                opcode if opcode == JavaOpCode::LAND => Box::new(just!(Ok(JavaOpCode::Land))),
                opcode if opcode == JavaOpCode::IOR => Box::new(just!(Ok(JavaOpCode::Ior))),
                opcode if opcode == JavaOpCode::LOR => Box::new(just!(Ok(JavaOpCode::Lor))),
                opcode if opcode == JavaOpCode::IXOR => Box::new(just!(Ok(JavaOpCode::Ixor))),
                opcode if opcode == JavaOpCode::LXOR => Box::new(just!(Ok(JavaOpCode::Lxor))),
                opcode if opcode == JavaOpCode::IINC => Box::new(comb::map(
                    sequence::pair(num::be_u8, num::be_i8),
                    |(index, delta)| Ok(JavaOpCode::Iinc { index, delta }),
                )),
                opcode if opcode == JavaOpCode::I2L => Box::new(just!(Ok(JavaOpCode::I2l))),
                opcode if opcode == JavaOpCode::I2F => Box::new(just!(Ok(JavaOpCode::I2f))),
                opcode if opcode == JavaOpCode::I2D => Box::new(just!(Ok(JavaOpCode::I2d))),
                opcode if opcode == JavaOpCode::L2I => Box::new(just!(Ok(JavaOpCode::L2i))),
                opcode if opcode == JavaOpCode::L2F => Box::new(just!(Ok(JavaOpCode::L2f))),
                opcode if opcode == JavaOpCode::L2D => Box::new(just!(Ok(JavaOpCode::L2d))),
                opcode if opcode == JavaOpCode::F2I => Box::new(just!(Ok(JavaOpCode::F2i))),
                opcode if opcode == JavaOpCode::F2L => Box::new(just!(Ok(JavaOpCode::F2l))),
                opcode if opcode == JavaOpCode::F2D => Box::new(just!(Ok(JavaOpCode::F2d))),
                opcode if opcode == JavaOpCode::D2I => Box::new(just!(Ok(JavaOpCode::D2i))),
                opcode if opcode == JavaOpCode::D2L => Box::new(just!(Ok(JavaOpCode::D2l))),
                opcode if opcode == JavaOpCode::D2F => Box::new(just!(Ok(JavaOpCode::D2f))),
                opcode if opcode == JavaOpCode::I2B => Box::new(just!(Ok(JavaOpCode::I2b))),
                opcode if opcode == JavaOpCode::I2C => Box::new(just!(Ok(JavaOpCode::I2c))),
                opcode if opcode == JavaOpCode::I2S => Box::new(just!(Ok(JavaOpCode::I2s))),
                opcode if opcode == JavaOpCode::LCMP => Box::new(just!(Ok(JavaOpCode::Lcmp))),
                opcode if opcode == JavaOpCode::FCMPL => Box::new(just!(Ok(JavaOpCode::Fcmpl))),
                opcode if opcode == JavaOpCode::FCMPG => Box::new(just!(Ok(JavaOpCode::Fcmpg))),
                opcode if opcode == JavaOpCode::DCMPL => Box::new(just!(Ok(JavaOpCode::Dcmpl))),
                opcode if opcode == JavaOpCode::DCMPG => Box::new(just!(Ok(JavaOpCode::Dcmpg))),
                opcode if opcode == JavaOpCode::IFEQ => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifeq { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFNE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifne { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFLT => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Iflt { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFGE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifge { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFGT => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifgt { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFLE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifle { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPEQ => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmpeq { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPNE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmpne { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPLT => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmplt { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPGE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmpge { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPGT => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmpgt { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ICMPLE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfIcmple { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ACMPEQ => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfAcmpeq { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IF_ACMPNE => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::IfAcmpne { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::GOTO => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Goto { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::JSR => Box::new(comb::map(num::be_i16, |offset| {
                    Ok(JavaOpCode::Jsr { offset })
                })),
                opcode if opcode == JavaOpCode::LOOKUPSWITCH => Box::new(comb::map(
                    sequence::pair(
                        sequence::preceded(bytes::take((idx + 1) % 4), num::be_i32),
                        comb::flat_map(num::be_i32, |num_pairs| {
                            multi::many_m_n(
                                num_pairs as usize,
                                num_pairs as usize,
                                sequence::pair(num::be_i32, num::be_i32),
                            )
                        }),
                    ),
                    |(default_offset, match_offsets)| {
                        Ok(JavaOpCode::Lookupswitch {
                            default_offset,
                            match_offsets,
                        })
                    },
                )),
                opcode if opcode == JavaOpCode::IRETURN => Box::new(just!(Ok(JavaOpCode::Ireturn))),
                opcode if opcode == JavaOpCode::LRETURN => Box::new(just!(Ok(JavaOpCode::Lreturn))),
                opcode if opcode == JavaOpCode::FRETURN => Box::new(just!(Ok(JavaOpCode::Freturn))),
                opcode if opcode == JavaOpCode::DRETURN => Box::new(just!(Ok(JavaOpCode::Dreturn))),
                opcode if opcode == JavaOpCode::ARETURN => Box::new(just!(Ok(JavaOpCode::Areturn))),
                opcode if opcode == JavaOpCode::GETSTATIC => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Getstatic {
                            field: pool.get_field_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::GETFIELD => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Getfield {
                            field: pool.get_field_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::PUTFIELD => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Putfield {
                            field: pool.get_field_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::INVOKEVIRTUAL => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Invokevirtual {
                            method: pool.get_method_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::INVOKESPECIAL => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Invokespecial {
                            method: pool.get_method_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::INVOKESTATIC => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Invokestatic {
                            method: pool.get_method_ref(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::INVOKEDYNAMIC => {
                    Box::new(comb::map(
                        // Gets a u16 then reads and ignores two zero bytes.
                        sequence::terminated(num::be_u16, bytes::tag(&[0, 0][..])),
                        |index| match pool.get(index)? {
                            &CPEntry::InvokeDynamic(bootstrap_idx, nat_idx) => {
                                let (name, r#type) = pool.get_name_and_type(nat_idx)?;
                                Ok(JavaOpCode::Invokedynamic {
                                    bootstrap_idx,
                                    name,
                                    r#type,
                                })
                            }
                            _ => unimplemented!(),
                        },
                    ))
                }
                opcode if opcode == JavaOpCode::NEW => Box::new(comb::map(num::be_u16, |index| {
                    Ok(JavaOpCode::New {
                        type_name: pool.get_class_name(index)?,
                    })
                })),
                opcode if opcode == JavaOpCode::NEWARRAY => {
                    Box::new(comb::map(PrimitiveValueType::nom_parse_cf, |r#type| {
                        Ok(JavaOpCode::Newarray { r#type })
                    }))
                }
                opcode if opcode == JavaOpCode::ANEWARRAY => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Anewarray {
                            el_type: pool.get_class_name(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::ARRAYLENGTH => {
                    Box::new(just!(Ok(JavaOpCode::Arraylength)))
                }
                opcode if opcode == JavaOpCode::ATHROW => Box::new(just!(Ok(JavaOpCode::Athrow))),
                opcode if opcode == JavaOpCode::CHECKCAST => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Checkcast {
                            type_name: pool.get_class_name(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::INSTANCEOF => {
                    Box::new(comb::map(num::be_u16, |index| {
                        Ok(JavaOpCode::Instanceof {
                            type_name: pool.get_class_name(index)?,
                        })
                    }))
                }
                opcode if opcode == JavaOpCode::MONITORENTER => {
                    Box::new(just!(Ok(JavaOpCode::Monitorenter)))
                }
                opcode if opcode == JavaOpCode::MONITOREXIT => {
                    Box::new(just!(Ok(JavaOpCode::Monitorexit)))
                }
                opcode if opcode == JavaOpCode::MULTIANEWARRAY => Box::new(comb::map(
                    sequence::pair(num::be_i16, num::be_u8),
                    |(index, dimensions)| {
                        if dimensions == 0 {
                            Err("`multianewarray` requires at least one dimension.")?
                        } else {
                            Ok(JavaOpCode::Multianewarray { index, dimensions })
                        }
                    },
                )),
                opcode if opcode == JavaOpCode::IFNULL => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifnull { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::IFNONNULL => {
                    Box::new(comb::map(num::be_i16, |offset| {
                        Ok(JavaOpCode::Ifnonnull { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::GOTO_W => {
                    Box::new(comb::map(num::be_i32, |offset| {
                        Ok(JavaOpCode::GotoW { offset })
                    }))
                }
                opcode if opcode == JavaOpCode::JSR_W => {
                    Box::new(comb::map(num::be_i32, |offset| {
                        Ok(JavaOpCode::JsrW { offset })
                    }))
                }
                opcode => unimplemented!("JavaOpCode::nom_parse: opcode == 0x{:2X}", opcode),
            };
            ret
        })(s)
    }
}

/// The body of a Java function.
#[derive(Clone, Debug, Default)]
pub struct JavaFunctionBody {
    instructions: HashMap<u16, JavaOpCode>,
    len: u16,
}

impl JavaFunctionBody {
    fn parse(s: &[u8], pool: &ConstantPool) -> CrateResult<Self> {
        Self::nom_parse_full(pool, s).map_err(CPAccessError::from)?
    }

    fn len(&self) -> u16 {
        self.len
    }

    fn set_len(&mut self, len: u16) {
        self.len = len;
    }

    fn add_instruction(&mut self, instruction: JavaOpCode) {
        let instr_len = instruction.len(self.len());
        self.instructions.insert(self.len(), instruction);
        self.set_len(self.len() + instr_len);
    }

    fn into_raw(mut self, pool: &mut ConstantPool) -> CPAccessResult<Vec<u8>> {
        let mut ret = vec![];
        let mut entries = self.instructions.drain().collect::<Vec<_>>();
        entries[..].sort_by_key(|entry| entry.0);
        for (idx, opcode) in entries {
            ret.extend(&opcode.into_raw(idx, pool)?[..])
        }
        Ok(ret)
    }
}

impl<'i, 'pool> NomParse<&'pool ConstantPool, &'i [u8]> for JavaFunctionBody {
    type Output = CrateResult<Self>;

    fn nom_parse(env: Self::Env, mut s: Self::Input) -> IResult<Self::Input, Self::Output> {
        let mut ret = Self::default();
        loop {
            match JavaOpCode::nom_parse((ret.len(), env), s) {
                Ok((rest, opcode)) => {
                    if rest == s {
                        return Err(Err::Error(<_ as ParseError<_>>::from_error_kind(
                            s,
                            ErrorKind::Many0,
                        )));
                    }
                    match opcode {
                        Ok(opcode) => ret.add_instruction(opcode),
                        Err(e) => return Ok((s, Err(e))),
                    }
                    s = rest;
                }
                Err(Err::Error(_)) => return Ok((s, Ok(ret))),
                Err(e) => return Err(e),
            }
        }
    }
}

/// An attribute of some object in a Java class file.
#[derive(Clone, Debug)]
pub enum JavaAttribute {
    /// A `long` value.
    ConstantLong(i64),
    /// A `float` value.
    ConstantFloat(f32),
    /// A `double` value.
    ConstantDouble(f64),
    /// An `int` value.
    ConstantInt(i32),
    /// A string value.
    ConstantString(String),
    /// A method body.
    Code {
        /// The maximum number of (4-byte) values on the argument stack of this
        /// function at any one time.
        max_stack: u16,
        /// The maximum number of local variables in use by this function at any
        /// one time.
        max_locals: u16,
        /// The actual code of the method.
        body: JavaFunctionBody,
        /// The exception handlers for the method.
        exception_handlers: Vec<ExceptionHandler>,
        /// The attributes of the method body.
        attributes: Vec<JavaAttribute>,
    },
    /// A stack map table is used for type checking.
    StackMapTable(Vec<StackMapFrame>),
    /// The list of exception types that the function is declared to throw.
    Exceptions(Vec<QualifiedClassName>),
    /// A list of non-package classes referred to or contained by this class.
    InnerClasses(Vec<InnerClassInfo>),
    /// The method containing this local or anonymous class.
    EnclosingMethod {
        /// The class containing this local or anonymous class.
        class: QualifiedClassName,
        /// The method of `class` containing this local or anonymous class.
        method: Option<(JavaIdentifier, JavaType)>,
    },
    /// An alternative to the synthetic flag on classes, methods, and fields.
    Synthetic,
    /// A non-erased generic type signature.
    Signature(String),
    /// The filename of the file that this class was compiled from.
    SourceFile(String),
    /// Extra debug information.
    SourceDebugExtension {
        /// The length of the debug information when encoded in JVM-8.
        jvm8_len: u32,
        /// The debug information.
        value: String,
    },
    /// Debug information about where in the `Code` attribute the line number of the original
    /// source code changes.
    LineNumberTable(Vec<LineNumber>),
    /// Debug information about local variables for a `Code` attribute.
    LocalVariableTable(Vec<LocalVariable>),
    /// An attribute that does not fall into any of the other categories.
    GenericAttribute {
        /// The name of the attribute.
        name: String,
        /// The value of the attribute.
        info: Vec<u8>,
    },
}

impl JavaAttribute {
    /// The name of the ConstantValue attribute.
    const CONSTANT_VALUE_NAME: &'static str = "ConstantValue";
    /// The name of the Code attribute.
    const CODE_NAME: &'static str = "Code";
    /// The name of the StackMapTable attribute.
    const STACK_MAP_TABLE_NAME: &'static str = "StackMapTable";
    /// The name of the Exceptions attribute.
    const EXCEPTIONS_NAME: &'static str = "Exceptions";
    /// The name of the InnerClasses attribute.
    const INNER_CLASSES_NAME: &'static str = "InnerClasses";
    /// The name of the EnclosingMethod attribute.
    const ENCLOSING_METHOD_NAME: &'static str = "EnclosingMethod";
    /// The name of the Synthetic attribute.
    const SYNTHETIC_NAME: &'static str = "Synthetic";
    /// The name of the Signature attribute.
    const SIGNATURE_NAME: &'static str = "Signature";
    /// The name of the SourceFile attribute.
    const SOURCE_FILE_NAME: &'static str = "SourceFile";
    /// The name of the SourceDebugExtension attribute.
    const SOURCE_DEBUG_EXTENSION_NAME: &'static str = "SourceDebugExtension";
    /// The name of the LineNumberTable attribute.
    const LINE_NUMBER_TABLE_NAME: &'static str = "LineNumberTable";
    /// The name of the LocalVariableTable attribute.
    const LOCAL_VARIABLE_TABLE_NAME: &'static str = "LocalVariableTable";

    /// Reads a single attribute from the byte source `src`.
    fn read(src: &mut dyn Read, pool: &ConstantPool, counter: &mut usize) -> CrateResult<Self> {
        let name_idx = read_u16(src, counter)?;
        match pool.get_utf8(name_idx)? {
            name if name == Self::CONSTANT_VALUE_NAME => {
                match read_u32(src, counter)? {
                    2 => {}
                    len => {
                        return Err(ClassParseError::InvalidAttributeLength {
                            name: name.to_string(),
                            len,
                        })?
                    }
                }
                let attr = match pool.get(read_u16(src, counter)?)? {
                    CPEntry::Integer(i) => Self::ConstantInt(*i),
                    CPEntry::Float(f) => Self::ConstantFloat(*f),
                    CPEntry::Long(l) => Self::ConstantLong(*l),
                    CPEntry::Double(d) => Self::ConstantDouble(*d),
                    CPEntry::String(idx) => Self::ConstantString(pool.get_owned_string(*idx)?),
                    entry => {
                        return Err(ClassParseError::InvalidAttributeValue {
                            name: name.to_string(),
                            value: entry.to_string(pool)?,
                        }
                        .into())
                    }
                };
                Ok(attr)
            }
            name if name == Self::CODE_NAME => {
                // Structure:
                // {
                //     name_idx: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     max_stack: u16,
                //     max_locals: u16,
                //     code_length: u32,
                //     code: [u8, code_length],
                //     exception_table_length: u16,
                //     exception_table: [ExceptionHandler; exception_table_length],
                //     attributes_count: u16,
                //     // Must include "StackMapTable" attribute.
                //     attributes: [JavaAttribute; attributes_count],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let max_stack = read_u16(src, counter)?;
                let max_locals = read_u16(src, counter)?;
                let code_len = read_u32(src, counter)?;
                let code = read_bytes(src, code_len.into(), counter)?;
                let body = JavaFunctionBody::parse(&code, pool)?;
                let exception_handlers = read_exception_handlers(src, pool, counter)?;
                let attributes = read_attributes(src, pool, counter)?;
                debug_assert_eq!(*counter - counter_base, value_length);
                let ret = Self::Code {
                    max_stack,
                    max_locals,
                    body,
                    exception_handlers,
                    attributes,
                };
                Ok(ret)
            }
            name if name == Self::STACK_MAP_TABLE_NAME => {
                // Structure:
                // {
                //     name_idx: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     entry_count: u16,
                //     entries: [StackMapFrame; entry_count],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let entry_count = read_u16(src, counter)?;
                let entries = (0..entry_count)
                    .map(|_| StackMapFrame::read(src, pool, counter))
                    .collect::<CrateResult<_>>()?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::StackMapTable(entries))
            }
            name if name == Self::EXCEPTIONS_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     number_of_exceptions: u16,
                //     exception_indices: [u16; number_of_exceptions],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let number_of_exceptions = read_u16(src, counter)?;
                let exception_types = (0..number_of_exceptions)
                    .map(|_| {
                        pool.get_class_name(read_u16(src, counter)?)
                            .map_err(|e| e.into())
                    })
                    .collect::<CrateResult<Vec<_>>>()?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::Exceptions(exception_types))
            }
            name if name == Self::INNER_CLASSES_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     number_of_classes: u16,
                //     classes_info: [
                //         {
                //             inner_class_idx: u16,
                //             outer_class_idx: u16,
                //             inner_name_idx: u16,
                //             inner_class_access_flags: u16,
                //         };
                //         number_of_classes
                //     ],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let num_classes = read_u16(src, counter)?;
                let classes_info = (0..num_classes)
                    .map(|_| InnerClassInfo::read(src, pool, counter))
                    .collect::<CrateResult<Vec<_>>>()?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::InnerClasses(classes_info))
            }
            name if name == Self::ENCLOSING_METHOD_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     class_index: u16,
                //     method_index: u16,
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let class_idx = read_u16(src, counter)?;
                let method_idx = read_u16(src, counter)?;
                debug_assert_eq!(value_length, *counter - counter_base);
                let class = pool.get_class_name(class_idx)?;
                let method = if method_idx == 0 {
                    None
                } else {
                    Some(pool.get_name_and_type(method_idx)?)
                };
                Ok(Self::EnclosingMethod { class, method })
            }
            name if name == Self::SYNTHETIC_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                // }
                let value_length = read_u32(src, counter)?;
                if value_length != 0 {
                    return Err(ClassParseError::InvalidAttributeLength {
                        name: Self::SYNTHETIC_NAME.into(),
                        len: value_length,
                    }
                    .into());
                }
                Ok(Self::Synthetic)
            }
            name if name == Self::SIGNATURE_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     signature_idx: u16,
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let signature_idx = read_u16(src, counter)?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::Signature(pool.get_utf8(signature_idx)?.to_string()))
            }
            name if name == Self::SOURCE_FILE_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     source_file_idx: u16,
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let source_file_idx = read_u16(src, counter)?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::SourceFile(
                    pool.get_utf8(source_file_idx)?.to_string(),
                ))
            }
            name if name == Self::SOURCE_DEBUG_EXTENSION_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     debug_extension: jvm8,
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let debug_extension = {
                    let mut bytes = Vec::with_capacity(value_length);
                    src.take(u64::try_from(value_length)?)
                        .read_to_end(&mut bytes)?;
                    crate::convert_jvm8_to_string(&bytes)?
                };
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::SourceDebugExtension {
                    jvm8_len: u32::try_from(value_length).unwrap(),
                    value: debug_extension,
                })
            }
            name if name == Self::LINE_NUMBER_TABLE_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     num_line_number_table_entries: u16,
                //     line_number_table: [LineNumber; num_line_number_table_entries],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let num_line_number_table_entries = read_u16(src, counter)?;
                let entries = (0..num_line_number_table_entries)
                    .map(|_| LineNumber::read(src, counter))
                    .collect::<CrateResult<Vec<_>>>()?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::LineNumberTable(entries))
            }
            name if name == Self::LOCAL_VARIABLE_TABLE_NAME => {
                // Structure:
                // {
                //     name: u16, // Already read
                //     value_length: u32, // Number of bytes in the remainder of the attribute
                //     local_variable_table_len: u16,
                //     local_variable_table: [LocalVariable; local_variable_table_len],
                // }
                let value_length = usize::try_from(read_u32(src, counter)?)?;
                let counter_base = *counter;
                let local_variable_table_len = read_u16(src, counter)?;
                let table = (0..local_variable_table_len)
                    .map(|_| LocalVariable::read(src, pool, counter))
                    .collect::<CrateResult<Vec<_>>>()?;
                debug_assert_eq!(value_length, *counter - counter_base);
                Ok(Self::LocalVariableTable(table))
            }
            "LocalVariableTypeTable" => unimplemented!("Attribute::LocalVariableTypeTable"),
            "Deprecated" => unimplemented!("Attribute::Deprecated"),
            "RuntimeVisibleAnnotations" => unimplemented!("Attribute::RuntimeVisibleAnnotations"),
            "RuntimeInvisibleAnnotations" => {
                unimplemented!("Attribute::RuntimeInvisibleAnnotations")
            }
            "RuntimeVisibleParameterAnnotations" => {
                unimplemented!("Attribute::RuntimeVisibleParameterAnnotations")
            }
            "RuntimeInvisibleParameterAnnotations" => {
                unimplemented!("Attribute::RuntimeInvisibleParameterAnnotations")
            }
            "AnnotationDefault" => unimplemented!("Attribute::AnnotationDefault"),
            "BootstrapMethods" => unimplemented!("Attribute::BootstrapMethods"),
            name => {
                let name = name.to_string();
                let info_length = eio::read_u32(src)?.into();
                let info = eio::read_bytes(src, info_length)?;
                Ok(Self::GenericAttribute { name, info })
            }
        }
    }

    /// Get the name of the attribute.
    pub fn name(&self) -> &str {
        match self {
            Self::ConstantLong(_)
            | Self::ConstantFloat(_)
            | Self::ConstantDouble(_)
            | Self::ConstantInt(_)
            | Self::ConstantString(_) => Self::CONSTANT_VALUE_NAME,
            Self::Code { .. } => Self::CODE_NAME,
            Self::StackMapTable(_) => Self::STACK_MAP_TABLE_NAME,
            Self::Exceptions(_) => Self::EXCEPTIONS_NAME,
            Self::InnerClasses(_) => Self::INNER_CLASSES_NAME,
            Self::EnclosingMethod { .. } => Self::ENCLOSING_METHOD_NAME,
            Self::Synthetic => Self::SYNTHETIC_NAME,
            Self::Signature(_) => Self::SIGNATURE_NAME,
            Self::SourceFile(_) => Self::SOURCE_FILE_NAME,
            Self::SourceDebugExtension { .. } => Self::SOURCE_DEBUG_EXTENSION_NAME,
            Self::LineNumberTable(_) => Self::LINE_NUMBER_TABLE_NAME,
            Self::LocalVariableTable(_) => Self::LOCAL_VARIABLE_TABLE_NAME,
            Self::GenericAttribute { name, .. } => name.as_ref(),
        }
    }

    /// Convert `self` into a form that can be directly written into a Java class file.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawAttribute> {
        match self {
            Self::ConstantLong(l) => {
                let name_idx = pool.add_utf8(Self::CONSTANT_VALUE_NAME.to_string())?;
                let value_idx = pool.add(CPEntry::Long(l))?;
                Ok(RawAttribute::ConstantValue {
                    name_idx,
                    value_idx,
                })
            }
            Self::ConstantFloat(f) => {
                let name_idx = pool.add_utf8(Self::CONSTANT_VALUE_NAME.to_string())?;
                let value_idx = pool.add(CPEntry::Float(f))?;
                Ok(RawAttribute::ConstantValue {
                    name_idx,
                    value_idx,
                })
            }
            Self::ConstantDouble(d) => {
                let name_idx = pool.add_utf8(Self::CONSTANT_VALUE_NAME.to_string())?;
                let value_idx = pool.add(CPEntry::Double(d))?;
                Ok(RawAttribute::ConstantValue {
                    name_idx,
                    value_idx,
                })
            }
            Self::ConstantInt(i) => {
                let name_idx = pool.add_utf8(Self::CONSTANT_VALUE_NAME.to_string())?;
                let value_idx = pool.add(CPEntry::Integer(i))?;
                Ok(RawAttribute::ConstantValue {
                    name_idx,
                    value_idx,
                })
            }
            Self::ConstantString(s) => {
                let name_idx = pool.add_utf8(Self::CONSTANT_VALUE_NAME.to_string())?;
                let value_idx = pool.add_string(s)?;
                Ok(RawAttribute::ConstantValue {
                    name_idx,
                    value_idx,
                })
            }
            Self::Code {
                max_stack,
                max_locals,
                body,
                exception_handlers,
                attributes,
            } => {
                let name_idx = pool.add_utf8(Self::CODE_NAME.to_string())?;
                let body = body.into_raw(pool)?;
                let exception_handlers = exception_handlers
                    .into_iter()
                    .filter_map(|handler| handler.into_raw(pool).ok())
                    .collect();
                let attributes = attributes
                    .into_iter()
                    .filter_map(|attribute| attribute.into_raw(pool).ok())
                    .collect();
                Ok(RawAttribute::Code {
                    name_idx,
                    max_stack,
                    max_locals,
                    body,
                    exception_handlers,
                    attributes,
                })
            }
            Self::StackMapTable(smt) => {
                let name_idx = pool.add_utf8(Self::STACK_MAP_TABLE_NAME.to_string())?;
                let entries = smt
                    .into_iter()
                    .map(|frame| frame.into_raw(pool))
                    .collect::<CrateResult<Vec<_>>>()?;
                Ok(RawAttribute::StackMapTable { name_idx, entries })
            }
            Self::Exceptions(exceptions) => {
                let name_idx = pool.add_utf8(Self::EXCEPTIONS_NAME.to_string())?;
                let exception_indices = exceptions
                    .into_iter()
                    .map(|exn| pool.add_class_name(exn))
                    .collect::<CPAccessResult<Vec<_>>>()?;
                Ok(RawAttribute::Exceptions {
                    name_idx,
                    exception_indices,
                })
            }
            Self::InnerClasses(classes_info) => {
                let name_idx = pool.add_utf8(Self::INNER_CLASSES_NAME.to_string())?;
                let classes_info = classes_info
                    .into_iter()
                    .map(|info| info.into_raw(pool))
                    .collect::<CrateResult<Vec<_>>>()?;
                Ok(RawAttribute::InnerClasses {
                    name_idx,
                    classes_info,
                })
            }
            Self::EnclosingMethod { class, method } => {
                let name_idx = pool.add_utf8(Self::ENCLOSING_METHOD_NAME.to_string())?;
                let class_idx = pool.add_class_name(class)?;
                let method_idx = method
                    .map(|(name, r#type)| pool.add_name_and_type(name, r#type))
                    .unwrap_or(Ok(0))?;
                Ok(RawAttribute::EnclosingMethod {
                    name_idx,
                    class_idx,
                    method_idx,
                })
            }
            Self::Synthetic => {
                let name_idx = pool.add_utf8(Self::SYNTHETIC_NAME.into())?;
                Ok(RawAttribute::Synthetic(name_idx))
            }
            Self::Signature(signature) => {
                let name_idx = pool.add_utf8(Self::SIGNATURE_NAME.to_string())?;
                let signature_idx = pool.add_utf8(signature)?;
                Ok(RawAttribute::Signature {
                    name_idx,
                    signature_idx,
                })
            }
            Self::SourceFile(source_file) => {
                let name_idx = pool.add_utf8(Self::SOURCE_FILE_NAME.to_string())?;
                let source_file_idx = pool.add_utf8(source_file)?;
                Ok(RawAttribute::SourceFile {
                    name_idx,
                    source_file_idx,
                })
            }
            Self::SourceDebugExtension { jvm8_len, value } => {
                let name_idx = pool.add_utf8(Self::SOURCE_DEBUG_EXTENSION_NAME.to_string())?;
                Ok(RawAttribute::SourceDebugExtension {
                    name_idx,
                    jvm8_len,
                    value,
                })
            }
            Self::LineNumberTable(table) => {
                let name_idx = pool.add_utf8(Self::LINE_NUMBER_TABLE_NAME.to_string())?;
                Ok(RawAttribute::LineNumberTable { name_idx, table })
            }
            Self::LocalVariableTable(table) => {
                let name_idx = pool.add_utf8(Self::LOCAL_VARIABLE_TABLE_NAME.to_string())?;
                let table = table
                    .into_iter()
                    .map(|entry| entry.into_raw(pool))
                    .collect::<CrateResult<Vec<_>>>()?;
                Ok(RawAttribute::LocalVariableTable { name_idx, table })
            }
            Self::GenericAttribute { name, info } => {
                let name_idx = pool.add_utf8(name)?;
                Ok(RawAttribute::GenericAttribute { name_idx, info })
            }
        }
    }
}

/// Read the attribute block for an element of a Java class file.
pub fn read_attributes(
    src: &mut dyn Read,
    pool: &ConstantPool,
    counter: &mut usize,
) -> CrateResult<Vec<JavaAttribute>> {
    let num_attributes = read_u16(src, counter)?;
    (0..num_attributes)
        .map(|_| JavaAttribute::read(src, pool, counter))
        .collect()
}

/// A description of a non-package class referred to by or contained in this class.
#[derive(Clone, Debug)]
pub struct InnerClassInfo {
    /// The full name of the inner class C.
    name: QualifiedClassName,
    /// The full name of the outer class. If C is not a member of a class or an interface - that is,
    /// if C is a top-level class or interface (JLS §7.6) or a local class (JLS §14.3) or an
    /// anonymous class (JLS §15.9.5) - then the value of the `container` must be `None`.
    ///
    /// Otherwise, the value of `container` must not equal the the value of `name`.
    container: Option<QualifiedClassName>,
    /// If C is anonymous (JLS §15.9.5), the value of `unqualified_name` must be `""`.
    ///
    /// Otherwise, the value of `unqualified_name` must be the original simple name of C, as given
    /// in the source code from which this class file was compiled.
    unqualified_name: String,
    /// The value of `access_flags` is a bitset of flags used to denote access permissions to and
    /// properties of class or interface C as declared in the source code from which this class file
    /// was compiled. It is used by a compiler to recover the original information when source code
    /// is not available.
    ///
    /// <table>
    /// <tr><th>Flag Name</th><th>Value</th><th>Interpretation</th></tr>
    /// <tr><td>ACC_PUBLIC</td><td>0x0001</td><td>Marked or implicitly public in source.</td></tr>
    /// <tr><td>ACC_PRIVATE</td><td>0x0002</td><td>Marked private in source.</td></tr>
    /// <tr><td>ACC_PROTECTED</td><td>0x0004</td><td>Marked protected in source.</td></tr>
    /// <tr><td>ACC_STATIC</td><td>0x0008</td><td>Marked or implicitly static in source.</td></tr>
    /// <tr><td>ACC_FINAL</td><td>0x0010</td><td>Marked or implicitly final in source.</td></tr>
    /// <tr><td>ACC_INTERFACE</td><td>0x0200</td><td>Was an interface in source.</td></tr>
    /// <tr>
    ///     <td>ACC_ABSTRACT</td><td>0x0400</td><td>Marked or implicitly abstract in source.</td>
    /// </tr>
    /// <tr>
    ///     <td>ACC_SYNTHETIC</td>
    ///     <td>0x1000</td>
    ///     <td>Declared synthetic; not present in the source code.</td>
    /// </tr>
    /// <tr><td>ACC_ANNOTATION</td><td>0x2000</td><td>Declared as an annotation interface.</td></tr>
    /// <tr><td>ACC_ENUM</td><td>0x4000</td><td>Declared as an enum class.</td></tr>
    /// </table>
    ///
    /// All bits of `access_flags` not assigned in the preceding table are reserved for future use.
    /// They should be set to zero in generated class files and should be ignored by Java Virtual
    /// Machine implementations.
    access_flags: u16,
}

impl InnerClassInfo {
    /// Convert this description into a form that can be written to a class file without further
    /// modifying the constant pool.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawInnerClassInfo> {
        let name_idx = pool.add_class_name(self.name)?;
        let container_idx = self
            .container
            .map(|qcn| pool.add_class_name(qcn))
            .unwrap_or(Ok(0))?;
        let unqualified_name_idx = if self.unqualified_name.is_empty() {
            0
        } else {
            pool.add_utf8(self.unqualified_name)?
        };
        Ok(RawInnerClassInfo {
            name_idx,
            container_idx,
            unqualified_name_idx,
            access_flags: self.access_flags,
        })
    }

    /// Read a description from a class file.
    fn read(src: &mut dyn Read, pool: &ConstantPool, counter: &mut usize) -> CrateResult<Self> {
        let name_idx = read_u16(src, counter)?;
        let container_idx = read_u16(src, counter)?;
        let unqualified_name_idx = read_u16(src, counter)?;
        let access_flags = read_u16(src, counter)?;
        let name = pool.get_class_name(name_idx)?;
        let container = if container_idx == 0 {
            None
        } else {
            Some(pool.get_class_name(container_idx)?)
        };
        let unqualified_name = if unqualified_name_idx == 0 {
            "".to_string()
        } else {
            pool.get_utf8(unqualified_name_idx)?.to_string()
        };
        Ok(Self {
            name,
            container,
            unqualified_name,
            access_flags,
        })
    }
}

impl AccessFlagged for InnerClassInfo {
    fn is_public(&self) -> bool {
        self.access_flags & 0x1 != 0
    }

    fn is_private(&self) -> bool {
        self.access_flags & 0x2 != 0
    }

    fn is_protected(&self) -> bool {
        self.access_flags & 0x4 != 0
    }

    fn is_static(&self) -> bool {
        self.access_flags & 0x8 != 0
    }

    fn is_final(&self) -> bool {
        self.access_flags & 0x10 != 0
    }

    fn is_super_special(&self) -> bool {
        false
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
        self.access_flags & 0x200 != 0
    }

    fn is_abstract(&self) -> bool {
        self.access_flags & 0x400 != 0
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

/// An entry in a [`JavaAttribute::LineNumberTable`] attribute.
#[derive(Clone, Copy, Debug)]
pub struct LineNumber {
    /// The offset into the `Code` attribute where line `line_number` starts.
    pub start_pc: u16,
    /// The line number of the source code that this entry describes.
    pub line_number: u16,
}

impl LineNumber {
    fn read(src: &mut dyn Read, counter: &mut usize) -> CrateResult<Self> {
        let start_pc = read_u16(src, counter)?;
        let line_number = read_u16(src, counter)?;
        Ok(Self {
            start_pc,
            line_number,
        })
    }
}

/// A local variable description in a `LocalVariableTable` attribute.
#[derive(Clone, Debug)]
pub struct LocalVariable {
    /// The index into the `Code` attribute of the first instruction where this local variable must
    /// have a value.
    start_pc: u16,
    /// The length of the slice into the `Code` attribute where this local variable must have a
    /// value.
    code_length: u16,
    /// The name of the local variable.
    name: String,
    /// The type of the local variable.
    r#type: JavaType,
    /// The index of the local variable in the current frame.
    index: u16,
}

impl LocalVariable {
    /// Reads a local variable description from `src`.
    fn read(src: &mut dyn Read, pool: &ConstantPool, counter: &mut usize) -> CrateResult<Self> {
        let start_pc = read_u16(src, counter)?;
        let code_length = read_u16(src, counter)?;
        let name_idx = read_u16(src, counter)?;
        let type_idx = read_u16(src, counter)?;
        let index = read_u16(src, counter)?;
        let name = pool.get_utf8(name_idx)?.to_string();
        let r#type = pool.get_type(type_idx)?;
        Ok(Self {
            start_pc,
            code_length,
            name,
            r#type,
            index,
        })
    }

    /// Converts a local variable description into a form that can be written without further
    /// modifying the constant pool.
    fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawLocalVariable> {
        let start_pc = self.start_pc;
        let code_length = self.code_length;
        let name_idx = pool.add_utf8(self.name)?;
        let type_idx = pool.add_type(self.r#type)?;
        let index = self.index;
        Ok(RawLocalVariable {
            start_pc,
            code_length,
            name_idx,
            type_idx,
            index,
        })
    }
}

/// A valid Java name. From [JLS7
/// §3.8](https://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.8), an Identifier is
/// any sequence of zero or more Unicode alphanumeric characters preceded by a Unicode alphabetic
/// character, where `_` (\u005F) and `$` (\u0024) are considered to be Unicode alphabetic
/// characters.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct JavaIdentifier(String);

impl JavaIdentifier {
    /// Try to interpret `s` as a Java identifier.
    pub fn new(s: &str) -> Option<Self> {
        s.parse().ok()
    }
}

impl Display for JavaIdentifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl<'i> NomParse<(), &'i str> for JavaIdentifier {
    type Output = Self;

    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self> {
        fn is_java_alphabetic(c: char) -> bool {
            c.is_alphabetic() || c == '_' || c == '$'
        }
        fn not_keyword(s: &str) -> bool {
            ![
                "null",
                "true",
                "false",
                "abstract",
                "assert",
                "boolean",
                "break",
                "byte",
                "case",
                "catch",
                "char",
                "class",
                "const",
                "continue",
                "default",
                "do",
                "double",
                "else",
                "enum",
                "extends",
                "final",
                "finally",
                "float",
                "for",
                "if",
                "goto",
                "implements",
                "import",
                "instanceof",
                "int",
                "interface",
                "long",
                "native",
                "new",
                "package",
                "private",
                "protected",
                "public",
                "return",
                "short",
                "static",
                "strictfp",
                "super",
                "switch",
                "synchronized",
                "this",
                "throw",
                "throws",
                "transient",
                "try",
                "void",
                "volatile",
                "while",
            ]
            .contains(&s)
        }
        comb::map(
            comb::verify(
                comb::recognize(sequence::pair(
                    comb::verify(character::anychar, |&c| is_java_alphabetic(c)),
                    bytes::take_while(|c| is_java_alphabetic(c) || c.is_numeric()),
                )),
                not_keyword,
            ),
            |s: &str| Self(s.to_string()),
        )(s)
    }
}

impl Borrow<str> for JavaIdentifier {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl_from_str_for_nom_parse_cf!(JavaIdentifier);

/// A Java package.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PackageName(Vec<JavaIdentifier>);

impl PackageName {
    /// The package that contains all classes not given explicit packages.
    pub const DEFAULT_PACKAGE: PackageName = PackageName(Vec::new());

    /// Create a new package name out of a sequence of valid Java identifiers.
    pub fn new(sections: Vec<JavaIdentifier>) -> PackageName {
        PackageName(sections)
    }

    /// Get the sequence of Java identifiers that make up the package name.
    pub fn sections(&self) -> &[JavaIdentifier] {
        &self.0[..]
    }

    /// Check whether `self` is the default package.
    pub fn is_default_package(&self) -> bool {
        self.sections().is_empty()
    }

    /// Convert `self` to the form used in Java class files.
    pub fn to_internal_form(&self) -> String {
        self.sections().join("/")
    }

    /// Convert `self` to the form used in Java source files.
    pub fn to_source_form(&self) -> String {
        self.sections().join(".")
    }
}

impl<'i> NomParse<(), &'i str> for PackageName {
    type Output = Self;

    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self::Output> {
        comb::map(
            branch::alt((
                multi::many0(sequence::terminated(
                    JavaIdentifier::nom_parse_cf,
                    bytes::tag("/"),
                )),
                multi::many0(sequence::terminated(
                    JavaIdentifier::nom_parse_cf,
                    bytes::tag("."),
                )),
            )),
            PackageName::new,
        )(s)
    }
}

impl_from_str_for_nom_parse_cf!(PackageName);

/// A fully-owned Java field object.
#[derive(Clone, Debug)]
pub struct JavaField {
    access_flags: u16,
    name: String,
    r#type: JavaType,
    attributes: Vec<JavaAttribute>,
}

impl JavaField {
    fn read(src: &mut dyn Read, pool: &ConstantPool) -> CrateResult<JavaField> {
        let access_flags = eio::read_u16(src)?;

        let name_idx = eio::read_u16(src)?;
        let name = pool.get_utf8(name_idx)?.to_string();

        let descriptor_idx = eio::read_u16(src)?;
        let r#type = pool.get_type(descriptor_idx)?;

        let attributes = read_attributes(src, pool, &mut 0)?;

        match JavaField::new(access_flags, name, r#type, attributes) {
            Some(f) => Ok(f),
            None => Err("Invalid type of field read from class file")?,
        }
    }

    /// Create a new fully owned Java field. `r#type` must not be a method type.
    pub fn new(
        access_flags: u16,
        name: String,
        r#type: JavaType,
        attributes: Vec<JavaAttribute>,
    ) -> Option<JavaField> {
        if let JavaType::Method { .. } = r#type {
            None
        } else {
            // TODO: Ensure that all attributes are applicable to fields
            Some(JavaField {
                access_flags,
                name,
                r#type,
                attributes,
            })
        }
    }

    /// Get the name of the field.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }

    /// Get the type of the field.
    pub fn r#type(&self) -> &JavaType {
        &self.r#type
    }

    /// Get the attributes of the field.
    pub fn attributes(&self) -> &[JavaAttribute] {
        &self.attributes[..]
    }

    /// Convert `self` into a form that can be directly written into a Java class file.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawField> {
        let access_flags = self.access_flags;
        let name_idx = pool.add_utf8(self.name)?;
        let type_idx = pool.add_type(self.r#type)?;
        let attributes = self
            .attributes
            .into_iter()
            .map(|attribute| attribute.into_raw(pool))
            .collect::<CrateResult<_>>()?;
        Ok(RawField {
            access_flags,
            name_idx,
            type_idx,
            attributes,
        })
    }
}

/// Read the fields block from a Java class file.
pub fn read_fields(src: &mut dyn Read, pool: &ConstantPool) -> CrateResult<Vec<JavaField>> {
    let num_fields = eio::read_u16(src)?;
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

/// Read the methods block from a Java class file.
pub fn read_methods(src: &mut dyn Read, pool: &ConstantPool) -> CrateResult<Vec<JavaMethod>> {
    let num_methods = eio::read_u16(src)?;
    let mut ret = Vec::with_capacity(num_methods.into());
    for _ in 0..num_methods {
        ret.push(JavaMethod::read(src, pool)?);
    }
    Ok(ret)
}

/// A fully-owned Java method object.
#[derive(Clone, Debug)]
pub struct JavaMethod {
    access_flags: u16,
    name: String,
    r#type: JavaType,
    attributes: Vec<JavaAttribute>,
}

impl JavaMethod {
    fn read(src: &mut dyn Read, pool: &ConstantPool) -> CrateResult<JavaMethod> {
        let access_flags = eio::read_u16(src)?;
        let name = pool.get_utf8(eio::read_u16(src)?)?.to_string();
        // TODO: implement check that `type` is a function type
        let r#type = pool.get_type(eio::read_u16(src)?)?;
        let attributes = read_attributes(src, pool, &mut 0)?;
        Ok(JavaMethod {
            access_flags,
            name,
            r#type,
            attributes,
        })
    }

    /// Get the name of the method.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the type of the method.
    pub fn r#type(&self) -> &JavaType {
        &self.r#type
    }

    /// Get the attributes of the method.
    pub fn attributes(&self) -> &[JavaAttribute] {
        &self.attributes[..]
    }

    /// Convert `self` into a structure that can be directly written into a Java class file.
    pub fn into_raw(self, pool: &mut ConstantPool) -> CrateResult<RawMethod> {
        let access_flags = self.access_flags;
        let name_idx = pool.add_utf8(self.name)?;
        let type_idx = pool.add_type(self.r#type)?;
        let attributes = self
            .attributes
            .into_iter()
            .map(|attribute| attribute.into_raw(pool))
            .collect::<CrateResult<_>>()?;
        Ok(RawMethod {
            access_flags,
            name_idx,
            type_idx,
            attributes,
        })
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

/// A version number.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct ClassFileVersion(u16, u16);

impl ClassFileVersion {
    /// Create a new version number.
    pub const fn new(major_version: u16, minor_version: u16) -> ClassFileVersion {
        ClassFileVersion(major_version, minor_version)
    }

    /// Get the major version number.
    pub const fn major_version(&self) -> u16 {
        self.0
    }

    /// Get the minor version number.
    pub const fn minor_version(&self) -> u16 {
        self.1
    }
}

fn read_u8(source: &mut dyn Read, counter: &mut usize) -> io::Result<u8> {
    let res = eio::read_u8(source)?;
    *counter += 1;
    Ok(res)
}

fn read_u16(source: &mut dyn Read, counter: &mut usize) -> io::Result<u16> {
    let res = eio::read_u16(source)?;
    *counter += 2;
    Ok(res)
}

fn read_u32(source: &mut dyn Read, counter: &mut usize) -> io::Result<u32> {
    let res = eio::read_u32(source)?;
    *counter += 4;
    Ok(res)
}

fn read_bytes(source: &mut dyn Read, length: u64, counter: &mut usize) -> io::Result<Vec<u8>> {
    let res = eio::read_bytes(source, length)?;
    *counter += res.len();
    Ok(res)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::io;

    use crate::NomFlatError;

    #[test]
    fn parses_aaload() -> Result<(), io::Error> {
        let src = [JavaOpCode::AALOAD];
        let expected = JavaOpCode::Aaload;
        let result = JavaOpCode::nom_parse((0, &Default::default()), &src[..])
            .map_err(NomFlatError::from)
            .map_err(crate::mk_io_error)?;
        assert_eq!(expected, result.1.map_err(crate::mk_io_error)?);
        Ok(())
    }
}
