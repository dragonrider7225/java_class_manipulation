// TODO: move java type parsers from parsers into parsers::java_type
// TODO: move other parsers into appropriate sub-package
use nom::{
    bytes::complete as bytes,
    combinator as comb,
    sequence as sequence,
};

use super::JavaType;

enum AvailableTypes {
    All,
    NoFunc,
    NoVoid,
    NoFuncNoVoid,
}

impl AvailableTypes {
    pub fn and_func(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc =>
                AvailableTypes::All,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoVoid,
        }
    }

    pub fn except_func(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc =>
                AvailableTypes::NoFunc,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFuncNoVoid,
        }
    }

    pub fn and_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid =>
                AvailableTypes::All,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFunc,
        }
    }

    pub fn except_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid =>
                AvailableTypes::NoVoid,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid =>
                AvailableTypes::NoFuncNoVoid,
        }
    }

    pub fn is_void_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc => true,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid => false,
        }
    }

    pub fn is_func_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => true,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid => false,
        }
    }
}

pub fn parse_byte(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Byte, bytes::tag("B"))(s)
}

pub fn parse_char(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Char, bytes::tag("C"))(s)
}

pub fn parse_double(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Double, bytes::tag("D"))(s)
}

pub fn parse_float(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Float, bytes::tag("F"))(s)
}

pub fn parse_int(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Int, bytes::tag("I"))(s)
}

pub fn parse_long(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Long, bytes::tag("J"))(s)
}

fn parse_package_name(s: &str) -> IResult<&str, &str> {
    // TODO: parse alpha
    // TODO: parse 0 or more alphanum
    // TODO: parse '/'
    // TODO: combine characters
}

fn parse_identifier(s: &str) -> IResult<&str, &str> {
    // TODO: parse alpha
    // TODO: parse 0 or more alphanum
    // TODO: combine characters
}

pub fn parse_class_name(s: &str) -> IResult<&str, JavaType> {
    // TODO: parse 0 or more parents
    // TODO: parse 1 identifier
    // TODO: combine parents with identifier

    // L((?:\alpha\alphanum*/)*\alpha\alphanum*); => Class(\1)
    // identifier: alpha alphanum*
    // alpha: filter(char::is_alphabetic, Unicode)
    // alphanum: filter(char::is_alphanum, Unicode)
    // absolute_class_id: (identifier '/')* identifier
}

pub fn parse_short(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Short, bytes::tag("S"))(s)
}

pub fn parse_bool(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Bool, bytes::tag("Z"))(s)
}

pub fn parse_array<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    comb::map(
        sequence::preceded(
            bytes::tag("["),
            JavaType::read_prefix_nom(available_types)
        ),
        JavaType::mk_array
    )
}

pub fn parse_func<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    comb::cond(
        available_types.is_func_available(),
        comb::map(
            sequence::pair(
                sequence::delimited(
                    bytes::tag("("),
                    multi::many0(
                        JavaType::read_prefix_nom(
                            available_types.except_func()
                        )
                    ),
                    bytes::tag(")")
                ),
                JavaType::read_prefix_nom(
                    available_types.except_func().and_void()
                )
            ),
            |(args, ret)| JavaType::Method(box args, box ret)
        )
    )
}

pub fn parse_void<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    comb::cond(
        available_types.is_void_available(),
        comb::value(JavaType::Void, bytes::tag("V"))
    )
}
