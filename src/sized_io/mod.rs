use std::io::{self, Error, Read, Write};

/**
 * Read a "big-endian" u8 from the specified bit source. Since big-endian and
 * little-endian refer to byte order, not bit order, there is no difference
 * between the big-endian and little-endian encodings of a single byte.
 * Nevertheless, both `read_u8` and `read_u8_le` are provided for the sake of
 * uniformity.
 */
pub fn read_u8(src: &mut dyn Read) -> io::Result<u8> {
    let mut buf = [0; 1];
    src.read_exact(&mut buf)?;
    Ok(u8::from_be_bytes(buf))
}

/**
 * Read a "big-endian" u8 from the specified bit source. Since big-endian and
 * little-endian refer to byte order, not bit order, there is no difference
 * between the big-endian and little-endian encodings of a single byte.
 * Nevertheless, both `read_u8` and `read_u8_le` are provided for the sake of
 * uniformity.
 */
pub fn read_u8_le(src: &mut dyn Read) -> io::Result<u8> {
    let mut buf = [0; 1];
    src.read_exact(&mut buf)?;
    Ok(u8::from_le_bytes(buf))
}

/// Read a big-endian u16 from the specified bit source.
pub fn read_u16(src: &mut dyn Read) -> io::Result<u16> {
    let mut buf = [0; 2];
    src.read_exact(&mut buf)?;
    Ok(u16::from_be_bytes(buf))
}

/// Read a little-endian u16 from the specified bit source.
pub fn read_u16_le(src: &mut dyn Read) -> io::Result<u16> {
    let mut buf = 0; 2];
    src.read_exact(&mut buf)?;
    Ok(u16::from_le_bytes(buf))
}

/// Read a big-endian u32 from the specified bit source.
pub fn read_u32(src: &mut dyn Read) -> io::Result<u32> {
    let mut buf = [0; 4];
    src.read_exact(&mut buf)?;
    Ok(u32::from_be_bytes(buf))
}

/// Read a little-endian u32 from the specified bit source.
pub fn read_u32_le(src: &mut dyn Read) -> io::Result<u32> {
    let mut buf = [0; 4];
    src.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

/// Read a big-endian u64 from the specified bit source.
pub fn read_u64(src: &mut dyn Read) -> io::Result<u64> {
    let mut buf = [0; 8];
    src.read_exact(&mut buf)?;
    Ok(u64::from_be_bytes(buf))
}

/// Read a little-endian u64 from the specified bit source.
pub fn read_u64_le(src: &mut dyn Read) -> io::Result<u64> {
    let mut buf = [0; 8];
    src.read_exact(&mut buf)?;
    Ok(u64::from_le_bytes(buf))
}

/// Read a big-endian u128 from the specified bit source.
pub fn read_u128(src: &mut dyn Read) -> io::Result<u128> {
    let mut buf = [0; 16];
    src.read_exact(&mut buf)?;
    Ok(u128::from_be_bytes(buf))
}

/// Read a little-endian u128 from the specified bit source.
pub fn read_u128_le(src: &mut dyn Read) -> io::Result<u128> {
    let mut buf = [0; 16];
    src.read_exact(&mut buf)?;
    Ok(u128::from_le_bytes(buf))
}

/// Read `length` bytes into a new `Vec` from the specified bit source.
pub fn read_bytes(src: &mut dyn Read, length: u64) -> io::Result<Vec<u8>> {
    let mut handle = src.take(length);
    let length_usize = usize::try_from(length).unwrap_or(usize::max_value());
    let mut buf = Vec::with_capacity(length_usize);
    if let Err(e) = handle.read_to_end(&mut buf) {
        let msg = format!(
            "Expected {} bytes, but ran into an error instead: {:?}",
            length, e);
        Err(Error::new(e.kind(), msg))
    } else {
        Ok(buf)
    }
}

/**
 * Write a "big-endian" u8 to the specified bit sink. Since big-endian and
 * little-endian refer to byte order, not bit order, there is no difference
 * between the big-endian and little-endian encodings of a single byte.
 * Nevertheless, both `write_u8` and `write_u8_le` are provided for the sake of
 * uniformity.
 */
pub fn write_u8(out: &mut dyn Write, val: u8) -> io::Result<()> {
    out.write_all(&val.to_be_bytes())
}

/**
 * Write a "little-endian" u8 to the specified bit sink. Since big-endian and
 * little-endian refer to byte order, not bit order, there is no difference
 * between the big-endian and little-endian encodings of a single byte.
 * Nevertheless, both `write_u8` and `write_u8_le` are provided for the sake of
 * uniformity.
 */
pub fn write_u8_le(out: &mut dyn Write, val: u8) -> io::Result<()> {
    out.write_all(&val.to_le_bytes())
}

/// Write a big-endian u16 to the specified bit sink.
pub fn write_u16(out: &mut dyn Write, val: u16) -> io::Result<()> {
    out.write_all(&val.to_be_bytes())
}

/// Write a little-endian u16 to the specified bit sink.
pub fn write_u16_le(out: &mut dyn Write, val: u16) -> io::Result<()> {
    out.write_all(&val.to_le_bytes())
}

/// Write a big-endian u32 to the specified bit sink.
pub fn write_u32(out: &mut dyn Write, val: u32) -> io::Result<()> {
    out.write_all(&val.to_be_bytes())
}

/// Write a little-endian u32 to the specified bit sink.
pub fn write_u32_le(out: &mut dyn Write, val: u32) -> io::Result<()> {
    out.write_all(&val.to_le_bytes())
}

/// Write a big-endian u64 to the specified bit sink.
pub fn write_u64(out: &mut dyn Write, val: u64) -> io::Result<()> {
    out.write_all(&val.to_be_bytes())
}

/// Write a little-endian u64 to the specified bit sink.
pub fn write_u64_le(out: &mut dyn Write, val: u64) -> io::Result<()> {
    out.write_all(&val.to_le_bytes())
}

/// Write a big-endian u128 to the specified bit sink.
pub fn write_u128(out: &mut dyn Write, val: u128) -> io::Result<()> {
    out.write_all(&val.to_be_bytes())
}

/// Write a little-endian u128 to the specified bit sink.
pub fn write_u128_le(out: &mut dyn Write, val: u128) -> io::Result<()> {
    out.write_all(&val.to_le_bytes())
}

/// Write the specified `Vec` of bytes to the specified bit sink.
pub fn write_bytes(out: &mut dyn Write, vals: Vec<u8>) -> io::Result<()> {
    out.write_all(&vals[..])
}

#[cfg(test)]
mod test {
    use super::*;

    use std::io::{Cursor, Error, ErrorKind};

    #[test]
    fn writes_8u8() -> io::Result<()> {
        let v = 8u8;
        let c = Cursor::new(Vec::with_capacity(1));
        write_u8(c, v)?;
        let buf = c.into_inner();
        match buf.get(0) {
            Some(8u8) => Ok(()),
            Some(x) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found {}",
                        v, x
                    )
                )
            ),
            None => Err(Error::new(ErrorKind::Other, "Write failed")),
        }
    }

    #[test]
    fn writes_8u8_le() -> io::Result<()> {
        let v = 8u8;
        let c = Cursor::new(Vec::with_capacity(1));
        write_u8_le(c, v)?;
        let buf = c.into_inner();
        match buf.get(0) {
            Some(8u8) => Ok(()),
            Some(x) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found {}",
                        v, x
                    )
                )
            ),
            None => Err(Error::new(ErrorKind::Other, "Write failed")),
        }
    }

    #[test]
    fn writes_4660u16() -> io::Result<()> {
        let v = 0x1234u16;
        let high_bits = 0x12u8;
        let low_bits = 0x34u8;
        let c = Cursor::new(Vec::with_capacity(2));
        write_u16(c, v)?;
        let buf = c.into_inner();
        match (buf.get(0), buf.get(1)) {
            (Some(0x12u8), Some(0x34u8)) => Ok(()),
            (Some(0x12u8), Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected second byte in buffer to be {}, found {}",
                        low_bits, y
                    )
                )
            ),
            (Some(0x12u8), None) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected second byte in buffer to be {}, found None",
                        low_bits
                    )
                )
            ),
            (Some(x), Some(0x34u8)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found {}",
                        high_bits, x
                    )
                )
            ),
            (None, Some(0x34u8)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found None",
                        high_bits
                    )
                )
            ),
            (Some(x), Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [{}, {}]",
                        high_bits, low_bits, x, y
                    )
                )
            ),
            (Some(x), None) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [{}, None]",
                        high_bits, low_bits, x
                    )
                )
            ),
            (None, Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [None, {}]",
                        high_bits, low_bits, y
                    )
                )
            ),
            (None, None) => Err(Error::new(ErrorKind::Other, "Write failed")),
        }
    }

    #[test]
    fn writes_4660u16_le() -> io::Result<()> {
        let v = 0x1234u16;
        let high_bits = 0x12u8;
        let low_bits = 0x34u8;
        let c = Cursor::new(Vec::with_capacity(2));
        write_u16(c, v)?;
        let buf = c.into_inner();
        match (buf.get(1), buf.get(0)) {
            (Some(0x12u8), Some(0x34u8)) => Ok(()),
            (Some(0x12u8), Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found {}",
                        low_bits, y
                    )
                )
            ),
            (Some(0x12u8), None) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected first byte in buffer to be {}, found None",
                        low_bits
                    )
                )
            ),
            (Some(x), Some(0x34u8)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected second byte in buffer to be {}, found {}",
                        high_bits, x
                    )
                )
            ),
            (None, Some(0x34u8)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected second byte in buffer to be {}, found None",
                        high_bits
                    )
                )
            ),
            (Some(x), Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [{}, {}]",
                        low_bits, high_bits, y, x
                    )
                )
            ),
            (Some(x), None) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [None, {}]",
                        low_bits, high_bits, x
                    )
                )
            ),
            (None, Some(y)) => Err(
                Error::new(
                    ErrorKind::Other,
                    format!(
                        "Expected buffer to be [{}, {}] found [{}, None]",
                        low_bits, high_bits, y
                    )
                )
            ),
            (None, None) => Err(Error::new(ErrorKind::Other, "Write failed")),
        }
    }
}
