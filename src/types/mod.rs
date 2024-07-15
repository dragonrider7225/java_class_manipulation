use nom::{
    branch, bytes::complete as bytes, combinator as comb, multi, sequence, Compare, IResult,
    InputLength, InputTake,
};

use crate::{
    fragment::PackageName,
    parsers::{impl_from_str_for_nom_parse_cf, NomBaseErr, NomParse, NomParseContextFree},
    Either, JavaIdentifier,
};

pub mod field;

/// A fully-qualified Java class name.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum QualifiedClassName {
    /// Any name of the form "path.to.ClassFile" (in a source file) or "path/to/ClassFile" (in a
    /// class file) is a fully-qualified Java class name.
    /// `.0` is the package which contains the class.
    /// `.1` is the unqualified name of the class.
    /// The class name "java.lang.String" would be broken up into "java.lang" as `.0` and "String"
    /// as `.1`.
    ClassFile(PackageName, JavaIdentifier),
    /// Any array type with element type which is either a primitive type or a fully-qualified Java
    /// class name is also a fully-qualified Java class name.
    Array(Either<PrimitiveValueType, Box<QualifiedClassName>>),
}

impl QualifiedClassName {
    /// Like [`ClassFile`] except that the arguments are combined into a single pair.
    ///
    /// [`ClassFile`]: #variant.ClassFile
    fn class_file_pair(name: (PackageName, JavaIdentifier)) -> QualifiedClassName {
        Self::ClassFile(name.0, name.1)
    }

    /// Create a type representing an array of the primitive type `el_type`.
    fn primitive_array(el_type: PrimitiveValueType) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Left(el_type))
    }

    /// Create a type representing an array of the class type `el_type`.
    fn class_array(el_type: QualifiedClassName) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Right(Box::new(el_type)))
    }

    /// Parse `s` as a non-array class name.
    pub(crate) fn from_full_str(s: &str) -> Result<QualifiedClassName, NomBaseErr<&str>> {
        Ok(Self::class_file_pair(
            comb::all_consuming(sequence::pair(
                PackageName::nom_parse_cf,
                JavaIdentifier::nom_parse_cf,
            ))(s)?
            .1,
        ))
    }

    /// Convert the type name to the form used in Java class files.
    pub(crate) fn to_internal_form(&self, delimit_class_name: bool) -> String {
        match self {
            QualifiedClassName::ClassFile(package, ucn) => {
                let prefix = if delimit_class_name { "L" } else { "" };
                let suffix = if delimit_class_name { ";" } else { "" };
                if package.is_default_package() {
                    format!("{prefix}{ucn}{suffix}")
                } else {
                    format!("{prefix}{}/{ucn}{suffix}", package.to_internal_form())
                }
            }
            QualifiedClassName::Array(Either::Left(primitive)) => {
                format!("[{}", primitive.to_internal_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("[{}", qcn.to_internal_form(delimit_class_name))
            }
        }
    }

    /// Convert the type name to the form used in Java source files.
    #[expect(dead_code)]
    pub(crate) fn to_source_form(&self) -> String {
        match self {
            QualifiedClassName::ClassFile(package, ucn) => {
                if package.is_default_package() {
                    format!("{}", ucn)
                } else {
                    format!("{}.{}", package.to_source_form(), ucn)
                }
            }
            QualifiedClassName::Array(Either::Left(primitive)) => {
                format!("{}[]", primitive.to_source_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("{}[]", qcn.to_source_form())
            }
        }
    }
}

impl<'i> NomParse<(), &'i str> for QualifiedClassName {
    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self> {
        branch::alt((
            comb::map(
                sequence::delimited(
                    bytes::tag("L"),
                    sequence::pair(PackageName::nom_parse_cf, JavaIdentifier::nom_parse_cf),
                    bytes::tag(";"),
                ),
                QualifiedClassName::class_file_pair,
            ),
            comb::map(
                sequence::preceded(
                    bytes::tag("["),
                    branch::alt((
                        comb::map(QualifiedClassName::nom_parse_cf, |x| {
                            Either::Right(Box::new(x))
                        }),
                        comb::map(PrimitiveValueType::nom_parse_cf, Either::Left),
                    )),
                ),
                QualifiedClassName::Array,
            ),
        ))(s)
    }
}

impl_from_str_for_nom_parse_cf!(QualifiedClassName);

/// An inhabited Java primitive type.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PrimitiveValueType {
    /// The Java equivalent to `bool`.
    Bool = 4,
    /// A UTF-16 character.
    Char = 5,
    /// The Java equivalent to `f32`.
    Float = 6,
    /// The Java equivalent to `f64`.
    Double = 7,
    /// The Java equivalent to `i8`.
    Byte = 8,
    /// The Java equivalent to `i16`.
    Short = 9,
    /// The Java equivalent to `i32`.
    Int = 10,
    /// The Java equivalent to `i64`.
    Long = 11,
}

impl PrimitiveValueType {
    pub(crate) fn to_internal_form(self) -> String {
        match self {
            Self::Bool => String::from("Z"),
            Self::Char => String::from("C"),
            Self::Float => String::from("F"),
            Self::Double => String::from("D"),
            Self::Byte => String::from("B"),
            Self::Short => String::from("S"),
            Self::Int => String::from("I"),
            Self::Long => String::from("J"),
        }
    }

    pub(crate) fn to_source_form(self) -> String {
        match self {
            Self::Bool => String::from("boolean"),
            Self::Char => String::from("char"),
            Self::Float => String::from("float"),
            Self::Double => String::from("double"),
            Self::Byte => String::from("byte"),
            Self::Short => String::from("short"),
            Self::Int => String::from("int"),
            Self::Long => String::from("long"),
        }
    }
}

impl<'i, I> NomParse<(), &'i I> for PrimitiveValueType
where
    I: ?Sized,
    for<'s> &'i I: Compare<&'s str>,
    &'i I: InputTake,
{
    type Output = Self;

    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self> {
        branch::alt((
            comb::value(Self::Bool, bytes::tag("Z")),
            comb::value(Self::Char, bytes::tag("C")),
            comb::value(Self::Float, bytes::tag("F")),
            comb::value(Self::Double, bytes::tag("D")),
            comb::value(Self::Byte, bytes::tag("B")),
            comb::value(Self::Short, bytes::tag("S")),
            comb::value(Self::Int, bytes::tag("I")),
            comb::value(Self::Long, bytes::tag("J")),
        ))(s)
    }
}

/// A primitive type in Java.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum JavaPrimitive {
    /// A special singleton type. Roughly equivalent to `()`.
    Void,
    /// A primitive type that is inhabited.
    ValueType(PrimitiveValueType),
}

impl JavaPrimitive {
    pub(crate) fn to_internal_form(self) -> String {
        match self {
            Self::Void => String::from("V"),
            Self::ValueType(v) => v.to_internal_form(),
        }
    }

    #[expect(dead_code, reason = "Print source code not yet implemented")]
    pub(crate) fn to_source_form(self) -> String {
        match self {
            Self::Void => String::from("void"),
            Self::ValueType(v) => v.to_source_form(),
        }
    }
}

impl<'i, I> NomParse<AvailableTypes, &'i I> for JavaPrimitive
where
    I: ?Sized,
    for<'s> &'i I: Compare<&'s str>,
    &'i I: InputLength,
    &'i I: InputTake,
{
    fn nom_parse(available_types: AvailableTypes, s: Self::Input) -> IResult<Self::Input, Self> {
        let mut value_type = comb::map(PrimitiveValueType::nom_parse_cf, Self::ValueType);
        if available_types.is_void_available() {
            branch::alt((comb::value(Self::Void, bytes::tag("V")), value_type))(s)
        } else {
            value_type(s)
        }
    }
}

impl<'i, I> NomParse<(), &'i I> for JavaPrimitive
where
    I: ?Sized,
    for<'s> &'i I: Compare<&'s str>,
    &'i I: InputLength,
    &'i I: InputTake,
{
    type Output = Self;

    fn nom_parse(_: (), i: Self::Input) -> IResult<Self::Input, Self> {
        <Self as NomParse<AvailableTypes, Self::Input>>::nom_parse(AvailableTypes::All, i)
    }
}

/// Determine which types should be permitted when parsing a `String` into a
/// `JavaPrimitive`.
#[derive(Clone, Copy)]
pub enum AvailableTypes {
    /// All Java types are permitted.
    All,
    /// All Java types except function types are permitted.
    NoFunc,
    /// All Java types except `void` are permitted.
    NoVoid,
    /// All Java types except function types and `void` are permitted.
    NoFuncNoVoid,
}

impl AvailableTypes {
    /// Given a set of available types, produce a set which is equivalent except that the result
    /// does not permit function types.
    pub fn except_func(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc => AvailableTypes::NoFunc,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid => AvailableTypes::NoFuncNoVoid,
        }
    }

    /// Given a set of available types, produce a set which is equivalent except that the result
    /// permits `void`.
    pub fn and_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => AvailableTypes::All,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid => AvailableTypes::NoFunc,
        }
    }

    /// Given a set of available types, produce a set which is equivalent except that the result
    /// does not permit `void`.
    pub fn except_void(&self) -> AvailableTypes {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => AvailableTypes::NoVoid,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid => AvailableTypes::NoFuncNoVoid,
        }
    }

    /// True iff `void` is permitted.
    pub fn is_void_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoFunc => true,
            AvailableTypes::NoVoid | AvailableTypes::NoFuncNoVoid => false,
        }
    }

    /// True iff function types are permitted.
    pub fn is_func_available(&self) -> bool {
        match self {
            AvailableTypes::All | AvailableTypes::NoVoid => true,
            AvailableTypes::NoFunc | AvailableTypes::NoFuncNoVoid => false,
        }
    }
}

/// A valid Java type.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum JavaType {
    /// A Java primitive.
    Primitive(JavaPrimitive),
    /// A class type.
    Class(QualifiedClassName),
    /// A function type.
    Method(JavaMethodType),
}

impl JavaType {
    /// Returns None if el_type is a method type or void, otherwise converts the type into a
    /// QualifiedClassName for array of el_type and wraps it in the JavaType::Class constructor.
    pub fn mk_array(el_type: JavaType) -> Option<Self> {
        let ret = match el_type {
            Self::Primitive(JavaPrimitive::Void) => None,
            Self::Primitive(JavaPrimitive::ValueType(el_type)) => {
                Some(QualifiedClassName::primitive_array(el_type))
            }
            Self::Class(el_type) => Some(QualifiedClassName::class_array(el_type)),
            Self::Method { .. } => None,
        };
        ret.map(Self::Class)
    }

    /// Returns None if any element of `arg_types` is void or a function type or if `ret_type` is a
    /// function type. Otherwise produces a `JavaType::Method` value.
    pub fn mk_method(arg_types: Vec<JavaType>, ret_type: JavaType) -> Option<Self> {
        JavaMethodType::new(arg_types, ret_type).map(Self::Method)
    }

    /// Test whether the type is one of the primitive types.
    #[expect(dead_code)]
    fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }

    /// Test whether the type is `void`.
    fn is_void(&self) -> bool {
        matches!(self, Self::Primitive(JavaPrimitive::Void))
    }

    /// Test whether the type is a class type&mdash;either a normal class or an array.
    #[expect(dead_code)]
    fn is_class(&self) -> bool {
        matches!(self, Self::Class(_))
    }

    /// Test whether the type is a function type.
    fn is_func(&self) -> bool {
        matches!(self, Self::Method { .. })
    }

    pub(crate) fn to_internal_form(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.to_internal_form(),
            Self::Class(qcn) => qcn.to_internal_form(true),
            Self::Method(jmt) => jmt.to_internal_form(),
        }
    }
}

impl<'i> NomParse<AvailableTypes, &'i str> for JavaType {
    fn nom_parse(available_types: AvailableTypes, s: Self::Input) -> IResult<Self::Input, Self> {
        branch::alt((
            comb::map(
                |s| JavaPrimitive::nom_parse(available_types, s),
                Self::Primitive,
            ),
            comb::map(QualifiedClassName::nom_parse_cf, Self::Class),
            comb::map(
                |s| JavaMethodType::nom_parse(available_types, s),
                Self::Method,
            ),
        ))(s)
    }
}

impl<'i> NomParse<(), &'i str> for JavaType {
    type Output = <Self as NomParse<AvailableTypes, &'i str>>::Output;

    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self::Output> {
        <Self as NomParse<AvailableTypes, &'i str>>::nom_parse(AvailableTypes::All, s)
    }
}

impl_from_str_for_nom_parse_cf!(JavaType);

// impl FromStr for JavaType {
//     type Err = NomFlatError;
//
//     fn from_str(s: &str) -> Result<JavaType, Self::Err> {
//         Ok(Self::nom_parse_full(AvailableTypes::All, s)?)
//     }
// }

/// A Java method type. Method argument and return types cannot themselves be method types and
/// method argument types also cannot be void.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct JavaMethodType {
    /// The argument types of the method.
    arg_types: Vec<JavaType>,
    /// The return type of the method.
    ret_type: Box<JavaType>,
}

impl JavaMethodType {
    /// Returns None if any element of `arg_types` is void or a function type or if `ret_type` is a
    /// function type.
    pub fn new(arg_types: Vec<JavaType>, ret_type: JavaType) -> Option<Self> {
        if arg_types
            .iter()
            .any(|arg_type| arg_type.is_func() || arg_type.is_void())
            || ret_type.is_func()
        {
            None
        } else {
            Some(Self {
                arg_types,
                ret_type: Box::new(ret_type),
            })
        }
    }

    pub fn to_internal_form(&self) -> String {
        format!(
            "({}){}",
            self.arg_types
                .iter()
                .map(JavaType::to_internal_form)
                .collect::<String>(),
            self.ret_type.to_internal_form(),
        )
    }
}

impl<'i> NomParse<AvailableTypes, &'i str> for JavaMethodType {
    fn nom_parse(env: AvailableTypes, s: &'i str) -> IResult<&'i str, Self::Output> {
        comb::map_opt(
            comb::cond(
                env.is_func_available(),
                comb::map_opt(
                    sequence::pair(
                        sequence::delimited(
                            bytes::tag("("),
                            multi::many0(|s| JavaType::nom_parse(AvailableTypes::NoFuncNoVoid, s)),
                            bytes::tag(")"),
                        ),
                        |s| JavaType::nom_parse(AvailableTypes::NoFunc, s),
                    ),
                    |(arg_types, ret_type)| Self::new(arg_types, ret_type),
                ),
            ),
            |x| x,
        )(s)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parser_fails_on_empty() {
        assert!("".parse::<JavaType>().is_err());
    }

    #[test]
    fn parses_byte() {
        let src = "B";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Byte,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_char() {
        let src = "C";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Char,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_double() {
        let src = "D";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Double,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_float() {
        let src = "F";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Float,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_int() {
        let src = "I";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Int,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_long() {
        let src = "J";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Long,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_class_name_default_package() {
        let src = "LClass;";
        let expected = Ok(JavaType::Class(QualifiedClassName::ClassFile(
            PackageName::DEFAULT_PACKAGE,
            JavaIdentifier::new("Class").unwrap(),
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[allow(non_snake_case, reason = "java.lang.Object is case-sensitive")]
    #[test]
    fn parses_class_name_Object() {
        let src = "Ljava/lang/Object;";
        let expected = Ok(JavaType::Class(QualifiedClassName::ClassFile(
            PackageName::new(vec![
                JavaIdentifier::new("java").unwrap(),
                JavaIdentifier::new("lang").unwrap(),
            ]),
            JavaIdentifier::new("Object").unwrap(),
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_short() {
        let src = "S";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Short,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_bool() {
        let src = "Z";
        let expected = Ok(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Bool,
        )));
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_array_int() {
        let src = "[I";
        let expected = JavaType::mk_array(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Int,
        )))
        .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_array_array_bool() {
        let src = "[[Z";
        let bool_array = JavaType::mk_array(JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Bool,
        )))
        .unwrap();
        let expected = JavaType::mk_array(bool_array).ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_func_thunk() {
        let src = "()V";
        let expected = JavaType::mk_method(vec![], JavaType::Primitive(JavaPrimitive::Void))
            .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_func_int_void() {
        let src = "(I)V";
        let arg_types = vec![JavaType::Primitive(JavaPrimitive::ValueType(
            PrimitiveValueType::Int,
        ))];
        let expected = JavaType::mk_method(arg_types, JavaType::Primitive(JavaPrimitive::Void))
            .ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }

    #[allow(non_snake_case, reason = "java.lang.Object is case-sensitive")]
    #[test]
    fn parses_func_array_int_int_Object() {
        let src = "([II)Ljava/lang/Object;";
        let arg_types = vec![
            JavaType::Class(QualifiedClassName::Array(Either::Left(
                PrimitiveValueType::Int,
            ))),
            JavaType::Primitive(JavaPrimitive::ValueType(PrimitiveValueType::Int)),
        ];
        let ret_type = JavaType::Class(QualifiedClassName::ClassFile(
            PackageName::new(vec![
                JavaIdentifier::new("java").unwrap(),
                JavaIdentifier::new("lang").unwrap(),
            ]),
            JavaIdentifier::new("Object").unwrap(),
        ));
        let expected = JavaType::mk_method(arg_types, ret_type).ok_or_else(Default::default);
        let result = src.parse();
        assert_eq!(expected, result);
    }
}
