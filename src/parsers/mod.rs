use nom::{
    combinator as comb,
    error::ParseError,
    IResult,
};

// TODO: move other parsers into appropriate sub-package

pub mod jvm8;

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

pub trait NomParse: Sized {
    fn nom_parse(s: &str) -> IResult<&str, Self>;
}

macro_rules! impl_from_str_for_nom_parse {
    ($($t:ty)*) => {$(
        impl FromStr for $t {
            type Err = ();

            fn from_str(s: &str) -> Result<$t, ()> {
                comb::all_consuming(<$t as NomParse>::nom_parse)(s).map(|x| x.1).map_err(|_| ())
            }
        }
    )*}
}
