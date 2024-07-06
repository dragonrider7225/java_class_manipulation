use std::{
    error::Error,
    fmt::{self, Debug, Display, Formatter},
};

use nom::{combinator as comb, error::ParseError, Err, IResult, InputLength, Parser};

// TODO: move other parsers into appropriate sub-package

pub mod jvm8;

pub(super) type NomBaseErr<I> = Err<nom::error::Error<I>>;

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

pub trait NomParse<Env, Input>: Sized {
    type Input = Input;
    type Output = Self;

    fn nom_parse(env: Env, s: Self::Input) -> IResult<Self::Input, Self::Output>;

    /// Like `Self::nom_parse`, but fails if `Self::nom_parse` does not consume its entire input.
    fn nom_parse_full(env: Env, s: Self::Input) -> Result<Self::Output, NomBaseErr<Self::Input>>
    where
        Env: Copy,
        Self::Input: InputLength,
    {
        Ok(comb::all_consuming(move |s| Self::nom_parse(env, s))(s)?.1)
    }
}

pub trait NomParseContextFree<Input>: NomParse<(), Input> {
    fn nom_parse_cf(s: Self::Input) -> IResult<Self::Input, Self::Output> {
        Self::nom_parse((), s)
    }

    fn nom_parse_full_cf(s: Self::Input) -> Result<Self::Output, NomBaseErr<Self::Input>>
    where
        Self::Input: InputLength,
    {
        Self::nom_parse_full((), s)
    }
}

// This impl covers every valid `T`, since the bounds on `T` are the same as the bounds on the
// types that can implement `NomParseContextFree`.
impl<T, Input> NomParseContextFree<Input> for T where T: NomParse<(), Input> {}

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

pub(crate) use impl_from_str_for_nom_parse_cf;
