use nom::{
    branch,
    bytes::complete as bytes,
    combinator as comb,
    Err,
    IResult,
    multi,
};

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

fn parse_one_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::map(
        comb::verify(bytes::take(1usize), is_jvm8_single_byte),
        |bytes: &[u8]| char::from(bytes[0]))(bytes)
}

fn parse_two_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::map(
        comb::verify(bytes::take(2usize), is_jvm8_double_byte),
        |bytes: &[u8]| {
            let high_bits = bytes[0] as u32 & 0x1F;
            let low_bits = bytes[1] as u32 & 0x3F;
            std::char::from_u32((high_bits << 6) | low_bits)
                .expect("Invalid character in JVM-8 string")
        })(bytes)
}

fn parse_three_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::map(
        comb::verify(bytes::take(3usize), is_jvm8_triple_byte),
        |bytes: &[u8]| {
            let high_bits = bytes[0] as u32 & 0xF;
            let mid_bits = bytes[1] as u32 & 0x3F;
            let low_bits = bytes[2] as u32 & 0x3F;
            std::char::from_u32((high_bits << 12) | (mid_bits << 6) | low_bits)
                .expect("Invalid character in JVM-8 string")
        })(bytes)
}

fn parse_six_byte_point(bytes: &[u8]) -> IResult<&[u8], char> {
    comb::map(
        comb::verify(bytes::take(6usize), is_jvm8_sextuple_byte),
        |bytes: &[u8]| {
            let high_high_bits = (bytes[1] as u32 & 0xF) + 0x1_0000;
            let low_high_bits = bytes[2] as u32 & 0x3F;
            let high_low_bits = bytes[4] as u32 & 0xF;
            let low_low_bits = bytes[5] as u32 & 0x3F;
            let high_bits = (high_high_bits << 6) | low_high_bits;
            let low_bits = (high_low_bits << 6) | low_low_bits;
            std::char::from_u32((high_bits << 10) | low_bits)
                .expect("Invalid character in JVM-8 string")
        })(bytes)
}

fn parse_jvm8_code_point(bytes: &[u8]) -> IResult<&[u8], char> {
    branch::alt((
        parse_one_byte_point,
        parse_two_byte_point,
        parse_three_byte_point,
        parse_six_byte_point))(bytes)
}

pub fn parse_jvm8(bytes: &[u8]) -> IResult<&[u8], String> {
    comb::all_consuming(
        multi::fold_many0(
            parse_jvm8_code_point,
            String::new(),
            |mut acc, c| {
                acc.push(c);
                acc
            }
        )
    )(bytes)
        .map_err(|e| {
            match e {
                Err::Incomplete(n) => Err::Incomplete(n),
                Err::Error((_, ek)) => Err::Error((bytes, ek)),
                Err::Failure((_, ek)) => Err::Error((bytes, ek)),
            }
        })
}
