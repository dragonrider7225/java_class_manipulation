fn parse_matching<I, E, P>(pred: P) -> impl Fn(I) -> IResult<I, char, E>
  where
    P: Fn(char) -> bool,
{
    //
}

fn parse_alphanumeric<I, E>(s: I) -> IResult<I, char, E>
  where
    I: InputIter + InputTake,
    E: ParseError<I>,
{
    take(1)
}
