use std::fmt::{self, Display, Formatter};

use nom::{branch, bytes::complete as bytes, combinator, multi, sequence};

use crate::{
    fragment::{JavaIdentifier, PackageName},
    parsers::{NomParse, NomParseContextFree},
    types::impl_from_str_for_nom_parse_cf,
    Either,
};

use super::PrimitiveValueType;

/// A type for a field, parameter, or local variable.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum JavaFieldType {
    /// A normal class type.
    Class(ClassType),
    /// An array type.
    Array(ArrayType),
    /// A type variable, like the `T` in `List<T>`.
    TypeVariable(JavaIdentifier),
}

impl Display for JavaFieldType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(ct) => write!(f, "{ct}"),
            Self::Array(at) => write!(f, "{at}"),
            Self::TypeVariable(jid) => write!(f, "T{jid};"),
        }
    }
}

impl<'i> NomParse<(), &'i str> for JavaFieldType {
    fn nom_parse(_: (), s: &'i str) -> nom::IResult<&'i str, Self::Output> {
        branch::alt((
            combinator::map(ClassType::nom_parse_cf, Self::Class),
            combinator::map(ArrayType::nom_parse_cf, Self::Array),
            combinator::map(
                sequence::delimited(
                    bytes::tag("T"),
                    JavaIdentifier::nom_parse_cf,
                    bytes::tag(";"),
                ),
                Self::TypeVariable,
            ),
        ))(s)
    }
}

impl_from_str_for_nom_parse_cf!(JavaFieldType);

/// A class type for [`JavaFieldType`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClassType {
    /// Either the package containing this class or the class where this class was defined.
    container: Either<PackageName, Box<Self>>,
    /// The simple name of this class type.
    name: SimpleClassType,
}

impl Display for ClassType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        /// The recursive part of converting a `ClassType` to a `String` so that the `L{};`
        /// delimiter doesn't get included multiple times.
        fn go(this: &ClassType, f: &mut Formatter<'_>) -> fmt::Result {
            match &this.container {
                Either::Left(package) => write!(f, "{}/", package.to_internal_form())?,
                Either::Right(parent) => {
                    go(parent, f)?;
                    write!(f, ".")?
                }
            }
            write!(f, "{}", this.name)
        }

        write!(f, "L")?;
        go(self, f)?;
        write!(f, ";")
    }
}

impl<'i> NomParse<(), &'i str> for ClassType {
    fn nom_parse(_: (), s: &'i str) -> nom::IResult<&'i str, Self::Output> {
        combinator::map(
            sequence::delimited(
                bytes::tag("L"),
                sequence::tuple((
                    multi::many0(sequence::terminated(
                        JavaIdentifier::nom_parse_cf,
                        bytes::tag("/"),
                    )),
                    SimpleClassType::nom_parse_cf,
                    multi::many0(sequence::preceded(
                        bytes::tag("."),
                        SimpleClassType::nom_parse_cf,
                    )),
                )),
                bytes::tag(";"),
            ),
            |(package, root, inner_classes)| {
                let root = Self {
                    container: Either::Left(PackageName::from(package)),
                    name: root,
                };
                inner_classes.into_iter().fold(root, |parent, inner| Self {
                    container: Either::Right(Box::new(parent)),
                    name: inner,
                })
            },
        )(s)
    }
}

/// An array type for [`JavaFieldType`].
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArrayType {
    /// The array's elements are some primitive type.
    Primitive(PrimitiveValueType),
    /// The array's elements are some reference type.
    Object(Box<JavaFieldType>),
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        match self {
            Self::Primitive(t) => write!(f, "{}", t.to_internal_form()),
            Self::Object(t) => write!(f, "{t}"),
        }
    }
}

impl<'i> NomParse<(), &'i str> for ArrayType {
    fn nom_parse(_: (), s: &'i str) -> nom::IResult<&'i str, Self::Output> {
        sequence::preceded(
            bytes::tag("["),
            branch::alt((
                combinator::map(PrimitiveValueType::nom_parse_cf, Self::Primitive),
                combinator::map(JavaFieldType::nom_parse_cf, |jft| {
                    Self::Object(Box::new(jft))
                }),
            )),
        )(s)
    }
}

/// An unqualified class name that has not been subjected to type erasure.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SimpleClassType {
    /// The simple name of the class.
    name: JavaIdentifier,
    /// The type arguments to the class.
    type_arguments: Vec<TypeArgument>,
}

impl Display for SimpleClassType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.type_arguments.is_empty() {
            write!(f, "<")?;
            self.type_arguments
                .iter()
                .try_for_each(|ta| write!(f, "{ta}"))?;
            write!(f, ">")
        } else {
            Ok(())
        }
    }
}

impl<'i> NomParse<(), &'i str> for SimpleClassType {
    fn nom_parse(_: (), s: &'i str) -> nom::IResult<&'i str, Self::Output> {
        combinator::map(
            sequence::pair(
                JavaIdentifier::nom_parse_cf,
                combinator::opt(sequence::delimited(
                    bytes::tag("<"),
                    multi::many1(TypeArgument::nom_parse_cf),
                    bytes::tag(">"),
                )),
            ),
            |(name, type_arguments)| Self {
                name,
                type_arguments: type_arguments.unwrap_or_else(Vec::new),
            },
        )(s)
    }
}

/// A type argument for [`SimpleClassType`].
#[derive(Clone, Debug, Eq, PartialEq)]
enum TypeArgument {
    /// A `?` wildcard.
    Wildcard,
    /// A `? super T` wildcard.
    Super(JavaFieldType),
    /// A `? extends T` wildcard.
    Sub(JavaFieldType),
    /// An exactly `T` type argument.
    Exact(JavaFieldType),
}

impl Display for TypeArgument {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Wildcard => write!(f, "*"),
            Self::Super(lb) => write!(f, "-{lb}"),
            Self::Sub(ub) => write!(f, "+{ub}"),
            Self::Exact(t) => write!(f, "{t}"),
        }
    }
}

impl<'i> NomParse<(), &'i str> for TypeArgument {
    fn nom_parse(_: (), s: &'i str) -> nom::IResult<&'i str, Self::Output> {
        branch::alt((
            combinator::value(Self::Wildcard, bytes::tag("*")),
            combinator::map(
                sequence::preceded(
                    combinator::opt(bytes::tag("+")),
                    JavaFieldType::nom_parse_cf,
                ),
                Self::Sub,
            ),
            combinator::map(
                sequence::preceded(
                    combinator::opt(bytes::tag("-")),
                    JavaFieldType::nom_parse_cf,
                ),
                Self::Super,
            ),
            combinator::map(JavaFieldType::nom_parse_cf, Self::Exact),
        ))(s)
    }
}
