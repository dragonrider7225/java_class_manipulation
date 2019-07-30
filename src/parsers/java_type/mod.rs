use nom::{
    branch,
    bytes::complete as bytes,
    combinator as comb,
    error::ParseError,
    multi,
    IResult,
    sequence,
};

use std::fmt::Display;

use super::super::JavaType;

#[derive(Clone, Copy)]
enum AvailableTypes {
    All,
    NoFunc,
    NoVoid,
    NoFuncNoVoid,
}

impl AvailableTypes {
    pub fn and_func(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc => AvailableTypes::All,
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
            AvailableTypes::All | AvailableTypes::NoVoid => AvailableTypes::All,
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

fn parse_byte(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Byte, bytes::tag("B"))(s)
}

fn parse_char(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Char, bytes::tag("C"))(s)
}

fn parse_double(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Double, bytes::tag("D"))(s)
}

fn parse_float(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Float, bytes::tag("F"))(s)
}

fn parse_int(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Int, bytes::tag("I"))(s)
}

fn parse_long(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Long, bytes::tag("J"))(s)
}

fn parse_alpha(s: &str) -> IResult<&str, char> {
    comb::verify(
        comb::map_opt(bytes::take(1usize), |cs: &str| cs.chars().next()),
        |c| c.is_alphabetic()
    )(s)
}

fn parse_alphanum(s: &str) -> IResult<&str, char> {
    comb::verify(
        comb::map_opt(bytes::take(1usize), |cs: &str| cs.chars().next()),
        |c| c.is_alphanumeric()
    )(s)
}

fn join_pair((left, right): (impl Display, impl Display)) -> String {
    format!("{}{}", left, right)
}

fn parse_package_name(s: &str) -> IResult<&str, String> {
    comb::map(
        sequence::pair(
            parse_alpha,
            comb::map(
                sequence::pair(
                    multi::fold_many0(
                        parse_alphanum,
                        String::new(),
                        |mut acc, c| {
                            acc.push(c);
                            acc
                        }
                    ),
                    bytes::tag("/")
                ),
                join_pair
            )
        ),
        join_pair
    )(s)
}

fn parse_identifier(s: &str) -> IResult<&str, String> {
    comb::map(
        sequence::pair(
            parse_alpha,
            multi::fold_many0(
                parse_alphanum,
                String::new(),
                |mut acc: String, c: char| {
                    acc.push(c);
                    acc
                }
            )
        ),
        join_pair
    )(s)
}

fn parse_class_name(s: &str) -> IResult<&str, JavaType> {
    comb::map(
        sequence::delimited(
            bytes::tag("L"),
            comb::map(
                sequence::pair(
                    multi::fold_many0(
                        parse_package_name,
                        String::new(),
                        |mut acc, s| {
                            acc.push_str(&s);
                            acc
                        }
                    ),
                    parse_identifier
                ),
                join_pair
            ),
            bytes::tag(";")
        ),
        |class_name| JavaType::Class(class_name)
    )(s)
}

fn parse_short(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Short, bytes::tag("S"))(s)
}

fn parse_bool(s: &str) -> IResult<&str, JavaType> {
    comb::value(JavaType::Bool, bytes::tag("Z"))(s)
}

fn parse_array<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    comb::map(
        sequence::preceded(
            bytes::tag("["),
            parse_prefix(available_types)
        ),
        JavaType::mk_array
    )
}

fn unwrap<I: Clone, O, E: ParseError<I>, F>(parser: F)
    -> impl Fn(I) -> IResult<I, O, E> 
  where
    F: Fn(I) -> IResult<I, Option<O>, E>,
{
    comb::map(comb::verify(parser, |o| o.is_some()), Option::unwrap)
}

fn sane_cond<I: Clone, O, E: ParseError<I>, F>(b: bool, parser: F)
    -> impl Fn(I) -> IResult<I, O, E>
  where
    F: Fn(I) -> IResult<I, O, E>,
{
    unwrap(comb::cond(b, parser))
}

fn parse_func<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    sane_cond(
        available_types.is_func_available(),
        comb::map(
            sequence::pair(
                sequence::delimited(
                    bytes::tag("("),
                    multi::many0(
                        parse_prefix(
                            available_types.except_func().except_void()
                        )
                    ),
                    bytes::tag(")")
                ),
                parse_prefix(available_types.except_func().and_void())
            ),
            move |(args, ret)| JavaType::mk_method(args, ret)
        )
    )
}

fn parse_void<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    sane_cond(
        available_types.is_void_available(),
        comb::value(JavaType::Void, bytes::tag("V"))
    )
}

fn parse_prefix<'a>(available_types: AvailableTypes)
        -> impl Fn(&'a str) -> IResult<&'a str, JavaType> {

    // Explicit anonymous function required to prevent "recursive function type"
    // error
    move |s| branch::alt((
        parse_byte,
        parse_char,
        parse_double,
        parse_float,
        parse_int,
        parse_long,
        parse_class_name,
        parse_short,
        parse_bool,
        parse_array(available_types),
        parse_func(available_types),
        parse_void(available_types)
    ))(s)
}

pub fn parse(s: &str) -> IResult<&str, JavaType> {
    comb::all_consuming(
        parse_prefix(AvailableTypes::All.except_func().except_void())
    )(s)
}

#[cfg(test)]
mod test {
    use super::*;

    fn fails_on_empty() {
        let result = parse(&"");
        let _ = result.expect_err("JavaType parser should fail on empty string");
    }

    fn parses_byte() {
        let src = "B";
        let target = "Byte";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Byte => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_char() {
        let src = "C";
        let target = "Char";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Char => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_double() {
        let src = "D";
        let target = "Double";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Double => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_float() {
        let src = "F";
        let target = "Float";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Float => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_int() {
        let src = "I";
        let target = "Int";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Int => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_long() {
        let src = "J";
        let target = "Long";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Long => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_class_name_default_package() {
        let src = "LClass;";
        let target = r#"Class("Class")"#;
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Class(class_name) => {
                match class_name.as_ref() {
                    "Class" => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    fn parses_class_name_Object() {
        let src = "Ljava/lang/Object;";
        let target = r#"Class("java/lang/Object")"#;
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Class(class_name) => {
                match class_name.as_ref() {
                    "java/lang/Object" => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    fn parses_short() {
        let src = "S";
        let target = "Short";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Short => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_bool() {
        let src = "Z";
        let target = "Bool";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Bool => {},
            r#type => panic!(format!("Expected {}, found {:?}", src, r#type)),
        }
    }

    fn parses_array_int() {
        let src = "[I";
        let target = "Array(Int)";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Array(box JavaType::Int) => {},
            result => panic!(format!("Expected {}, found {:?}", target, result)),
        }
    }

    fn parses_array_array_bool() {
        let src = "[[Z";
        let target = "Array(Array(Int))";
        match parse(&src).expect(&format!("{} should be {}", src, target)).1 {
            JavaType::Array(box JavaType::Array(box JavaType::Bool)) => {},
            result => panic!(format!("Expected {}, found {:?}", target, result)),
        }
    }

    fn parses_func_thunk() {
        let src = "()V";
        let target = "Method([], Void)";
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(args, box JavaType::Void) => {
                match &(*args)[..] {
                    &[] => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    fn parses_func_int_void() {
        let src = "(I)V";
        let target = "Method([Int], Void)";
        let result = parse(&src).expect(&format!("{} should be {}", src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(args, box JavaType::Void) => {
                match &(*args)[..] {
                    &[JavaType::Int] => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }

    fn parses_func_array_int_int_Object() {
        let src = "([II)Ljava/lang/Object;";
        let target = r#"Method([Array(Int), Int], Class("java/lang/Object")"#;
        let result = parse(&src)
            .expect(&format!(r#"{} should be {}"#, src, target))
            .1;
        let panic_str = format!("Expected {}, found {:?}", target, result);
        match result {
            JavaType::Method(ref args, box JavaType::Class(ref class_name)) => {
                match (&(*args)[..], class_name.as_ref()) {
                    (&[JavaType::Array(box JavaType::Int), JavaType::Int], "java/lang/Object") => {},
                    _ => panic!(panic_str),
                }
            },
            _ => panic!(panic_str),
        }
    }
}
