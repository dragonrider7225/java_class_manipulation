use nom::{
    branch, bytes::complete as bytes, combinator as comb, error::Error, multi, sequence, Compare,
    IResult, InputLength, InputTake, Parser,
};

use crate::{
    fragment::PackageName,
    parsers::{self, NomBaseErr, NomParse, NomParseContextFree},
    Either, JavaIdentifier,
};

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
    fn class_file(package: PackageName, class: JavaIdentifier) -> QualifiedClassName {
        QualifiedClassName::ClassFile(package, class)
    }

    fn class_file_pair(name: (PackageName, JavaIdentifier)) -> QualifiedClassName {
        Self::class_file(name.0, name.1)
    }

    fn primitive_array(el_type: PrimitiveValueType) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Left(el_type))
    }

    fn class_array(el_type: QualifiedClassName) -> QualifiedClassName {
        QualifiedClassName::Array(Either::Right(box el_type))
    }

    pub(crate) fn from_full_str(s: &str) -> Result<QualifiedClassName, NomBaseErr<&str>> {
        Ok(Self::class_file_pair(
            comb::all_consuming(sequence::pair(
                PackageName::nom_parse_cf,
                JavaIdentifier::nom_parse_cf,
            ))(s)?
            .1,
        ))
    }

    pub(crate) fn to_internal_form(&self) -> String {
        match self {
            QualifiedClassName::ClassFile(package, ucn) => {
                if package.is_default_package() {
                    format!("{}", ucn)
                } else {
                    format!("{}/{}", package.to_internal_form(), ucn)
                }
            }
            QualifiedClassName::Array(Either::Left(primitive)) => {
                format!("[{}", primitive.to_internal_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("[{}", qcn.to_internal_form())
            }
        }
    }

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
                format!("{}[]", primitive._to_source_form())
            }
            QualifiedClassName::Array(Either::Right(box qcn)) => {
                format!("{}[]", qcn.to_source_form())
            }
        }
    }
}

impl<'i> NomParse<(), &'i str> for QualifiedClassName {
    type Output = Self;

    fn nom_parse(_: (), s: Self::Input) -> IResult<Self::Input, Self> {
        branch::alt((
            comb::map(
                sequence::delimited(
                    bytes::tag("L"),
                    sequence::pair(PackageName::nom_parse_cf, JavaIdentifier::nom_parse_cf),
                    bytes::tag(";"),
                ),
                |(package, name)| QualifiedClassName::class_file(package, name),
            ),
            comb::map(
                sequence::preceded(
                    bytes::tag("["),
                    branch::alt((
                        comb::map(QualifiedClassName::nom_parse_cf, |x| Either::Right(box x)),
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
    pub(crate) fn to_internal_form(&self) -> String {
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

    pub(crate) fn _to_source_form(&self) -> String {
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
    pub(crate) fn to_internal_form(&self) -> String {
        match self {
            Self::Void => String::from("V"),
            Self::ValueType(v) => v.to_internal_form(),
        }
    }

    pub(crate) fn _to_source_form(&self) -> String {
        match self {
            Self::Void => String::from("void"),
            Self::ValueType(v) => v._to_source_form(),
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
    type Output = Self;

    fn nom_parse(available_types: Self::Env, s: Self::Input) -> IResult<Self::Input, Self> {
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
    Method {
        /// The argument types of the method.
        arg_types: Box<Vec<JavaType>>,
        /// The return type of the method.
        ret_type: Box<JavaType>,
    },
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
        ret.map(|x| Self::Class(x))
    }

    /// Returns None if any element of `arg_types` is void or a function type or if `ret_type` is a
    /// function type. Otherwise produces a `JavaType::Method` value.
    pub fn mk_method(arg_types: Vec<JavaType>, ret_type: JavaType) -> Option<Self> {
        for arg_type in &arg_types {
            if arg_type.is_func() || arg_type.is_void() {
                return None;
            }
        }
        if ret_type.is_func() {
            None
        } else {
            Some(Self::Method {
                arg_types: box arg_types,
                ret_type: box ret_type,
            })
        }
    }

    fn nom_method<'a>(
        available_types: AvailableTypes,
    ) -> impl Parser<&'a str, Self, Error<&'a str>> {
        let arg_types = available_types.except_func().except_void();
        let ret_types = available_types.except_func().and_void();
        let arg_types = sequence::delimited(
            bytes::tag("("),
            multi::many0(move |s| JavaType::nom_parse(arg_types, s)),
            bytes::tag(")"),
        );
        parsers::sane_cond(
            available_types.is_func_available(),
            comb::map_opt(
                sequence::pair(arg_types, move |s| JavaType::nom_parse(ret_types, s)),
                |(arg_types, ret_type)| Self::mk_method(arg_types, ret_type),
            ),
        )
    }

    fn _is_primitive(&self) -> bool {
        match self {
            Self::Primitive(_) => true,
            _ => false,
        }
    }

    fn is_void(&self) -> bool {
        match self {
            Self::Primitive(JavaPrimitive::Void) => true,
            _ => false,
        }
    }

    fn _is_class(&self) -> bool {
        match self {
            Self::Class(_) => true,
            _ => false,
        }
    }

    fn is_func(&self) -> bool {
        match self {
            Self::Method { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn to_internal_form(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.to_internal_form(),
            Self::Class(qcn) => match qcn {
                QualifiedClassName::ClassFile(..) => format!("L{};", qcn.to_internal_form()),
                _ => qcn.to_internal_form(),
            },
            Self::Method {
                arg_types,
                ret_type,
            } => format!(
                "({}){}",
                arg_types
                    .iter()
                    .map(JavaType::to_internal_form)
                    .collect::<String>(),
                ret_type.to_internal_form(),
            ),
        }
    }
}

impl<'i> NomParse<AvailableTypes, &'i str> for JavaType {
    type Output = Self;

    fn nom_parse(available_types: Self::Env, s: Self::Input) -> IResult<Self::Input, Self> {
        branch::alt((
            comb::map(
                |s| JavaPrimitive::nom_parse(available_types, s),
                JavaType::Primitive,
            ),
            comb::map(QualifiedClassName::nom_parse_cf, JavaType::Class),
            JavaType::nom_method(available_types),
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
