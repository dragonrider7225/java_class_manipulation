use std::{error::Error, fmt::{self, Debug, Display, Formatter}};

use nom::{combinator as comb, error::{ErrorKind, ParseError}, Err, IResult, InputLength};

// TODO: move other parsers into appropriate sub-package

pub mod jvm8;

pub(super) type NomBaseErr<I> = Err<(I, ErrorKind)>;

/// An error resulting from a failed attempt to use `nom` to parse a string into a value.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct NomFlatError {
    e: String,
}

impl Display for NomFlatError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.e)
    }
}

impl<E: Debug> From<Err<E>> for NomFlatError {
    fn from(e: Err<E>) -> Self {
        Self {
            e: format!("{:?}", e),
        }
    }
}

impl Error for NomFlatError {}

macro_rules! just {
    ($v:expr) => {
        |bytes| Ok((bytes, $v))
    };
}

pub fn unwrap<E, F, I, O>(parser: F) -> impl Fn(I) -> IResult<I, O, E>
where
    E: ParseError<I>,
    F: Fn(I) -> IResult<I, Option<O>, E>,
    I: Clone,
{
    comb::map_opt(parser, |x| x)
}

pub fn sane_cond<E, F, I, O>(b: bool, parser: F) -> impl Fn(I) -> IResult<I, O, E>
where
    E: ParseError<I>,
    F: Fn(I) -> IResult<I, O, E>,
    I: Clone,
{
    unwrap(comb::cond(b, parser))
}

pub trait NomParse<Env, Input>: Sized {
    type Env = Env;
    type Input = Input;
    type Output;

    fn nom_parse(env: Env, s: Input) -> IResult<Input, Self::Output>;

    /// Like `Self::nom_parse`, but fails if `Self::nom_parse` does not consume its entire input.
    fn nom_parse_full(env: Env, s: Input) -> Result<Self::Output, NomBaseErr<Input>>
    where
        Env: Copy,
        Input: InputLength,
    {
        Ok(comb::all_consuming(move |s| Self::nom_parse(env, s))(s)?.1)
    }
}

pub trait NomParseContextFree<Input>: NomParse<(), Input> {
    fn nom_parse_cf(s: Input) -> IResult<Input, Self::Output> {
        Self::nom_parse((), s)
    }

    fn nom_parse_full_cf(s: Input) -> Result<Self::Output, NomBaseErr<Input>>
    where
        Input: InputLength,
    {
        Self::nom_parse_full((), s)
    }
}

// This impl covers every valid `T`, since the bounds on `T` are the same as the bounds on the
// types that can implement `NomParseContextFree`.
impl<T, Input> NomParseContextFree<Input> for T
where
    T: NomParse<(), Input>,
{
}

macro_rules! impl_from_str_for_nom_parse_cf {
    ($($t:ty)*) => {$(
        impl ::std::str::FromStr for $t {
            type Err = $crate::parsers::NomFlatError;

            fn from_str<'a>(s: &'a str) -> ::std::result::Result<$t, Self::Err> {
                Ok(<$t as $crate::parsers::NomParseContextFree<&'a str>>::nom_parse_full_cf(s)?)
            }
        }
    )*}
}
