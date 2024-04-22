use std::{fmt::Display, ops::Neg};

use super::{
    ast::{Expression, Number, Operation, Operator},
    tokenize::{Token, TokenKind},
};

pub(crate) type ParseResult<'a, T> = Result<(&'a [Token], usize, T), (&'a [Token], ParseError)>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ParseError {
    index: usize,
    kind: ParseErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ParseErrorKind {
    TooSmallNegativeInt,
    ExpectNumberButOthers,
    ExpectNumberButEmpty,
    ParenthesesIsNotClosed,
    UnexpectedToken,
    InvalidExpressionInParentheses,
}

impl ParseError {
    pub fn new(index: usize, kind: ParseErrorKind) -> ParseError {
        ParseError { index, kind }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn kind(&self) -> &ParseErrorKind {
        &self.kind
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = "構文解析エラー: ";
        let index = self.index() + 1;

        match self.kind() {
            ParseErrorKind::TooSmallNegativeInt => {
                write!(f, "{}負の整数が小さすぎました {}文字目", tag, index)
            }
            ParseErrorKind::ExpectNumberButOthers => {
                write!(
                    f,
                    "{}数字があるべき場所に違う文字がありました {}文字目",
                    tag, index
                )
            }
            ParseErrorKind::ExpectNumberButEmpty => {
                write!(f, "{}数字があるべき場所に何もありませんでした", tag)
            }
            ParseErrorKind::ParenthesesIsNotClosed => {
                write!(f, "{}括弧が閉じていません {}文字目", tag, index)
            }
            ParseErrorKind::UnexpectedToken => {
                write!(f, "{}予期せぬ文字があります {}文字目", tag, index)
            }
            ParseErrorKind::InvalidExpressionInParentheses => {
                write!(f, "{}括弧内の式が不正です {}文字目", tag, index)
            }
        }
    }
}

pub(super) fn parse(tokens: &[Token]) -> Result<Expression, Vec<ParseError>> {
    let mut errors = Vec::new();

    let (remain, opt_expr) = match parse_operation_add_sub(tokens, &mut errors) {
        Ok((remain, _, expr)) => (remain, Some(expr)),
        Err((remain, error)) => {
            errors.push(error);
            (remain, None)
        }
    };

    for t in remain {
        errors.push(ParseError::new(t.index(), ParseErrorKind::UnexpectedToken));
    }

    match opt_expr {
        Some(expr) if errors.is_empty() => Ok(expr),
        _ => Err(errors),
    }
}

fn parse_operation_add_sub<'a>(
    tokens: &'a [Token],
    errors: &mut Vec<ParseError>,
) -> ParseResult<'a, Expression> {
    let (mut remain, index, mut expr) = parse_operation_mul_div_rem(tokens, errors)?;

    while let Some(t) = remain.first() {
        match t.kind() {
            TokenKind::Operator(o) if o == &Operator::Add || o == &Operator::Sub => {
                let (rem, _, operand_2) =
                    parse_operation_mul_div_rem(remain.split_at(1).1, errors)?;
                remain = rem;
                expr = Expression::Operation(Operation::new(
                    Box::new(expr),
                    Box::new(operand_2),
                    o.clone(),
                ));
            }
            _ => break,
        }
    }

    Ok((remain, index, expr))
}

fn parse_operation_mul_div_rem<'a>(
    tokens: &'a [Token],
    errors: &mut Vec<ParseError>,
) -> ParseResult<'a, Expression> {
    let (mut remain, index, mut expr) = parse_parentheses(tokens, errors)?;

    while let Some(t) = remain.first() {
        match t.kind() {
            TokenKind::Operator(o)
                if o == &Operator::Mul || o == &Operator::Div || o == &Operator::Rem =>
            {
                let (rem, _, operand_2) = parse_parentheses(remain.split_at(1).1, errors)?;
                remain = rem;
                expr = Expression::Operation(Operation::new(
                    Box::new(expr),
                    Box::new(operand_2),
                    o.clone(),
                ));
            }
            _ => break,
        }
    }

    Ok((remain, index, expr))
}

fn parse_parentheses<'a>(
    tokens: &'a [Token],
    errors: &mut Vec<ParseError>,
) -> ParseResult<'a, Expression> {
    let (remain, paren_open_index) = match tokens.first().map(|t| (t.index(), t.kind())) {
        Some((i, TokenKind::ParenthesesOpen)) => (tokens.split_at(1).1, i),
        _ => {
            return parse_signed_number(tokens)
                .map(|(remain, index, number)| (remain, index, Expression::Number(number)))
        }
    };

    match parse_operation_add_sub(remain, errors) {
        Ok((remain, _, body)) => {
            let (remain, _, _) = parse_parentheses_close(remain, errors, paren_open_index)?;
            Ok((
                remain,
                paren_open_index,
                Expression::Parentheses(Box::new(body)),
            ))
        }
        Err((remain, error)) => {
            let (remain, _, _) = parse_parentheses_close(remain, errors, paren_open_index)?;
            Err((remain, error))
        }
    }
}

fn parse_parentheses_close<'a>(
    tokens: &'a [Token],
    errors: &mut Vec<ParseError>,
    paren_open_index: usize,
) -> ParseResult<'a, ()> {
    let mut remain = tokens;
    let mut new_errors = Vec::new();

    while let Some(token) = remain.first() {
        remain = remain.split_at(1).1;
        match token.kind() {
            TokenKind::ParenthesesClose => {
                if new_errors.is_empty() {
                    return Ok((remain, token.index(), ()));
                } else {
                    errors.extend(new_errors);
                    return Err((
                        remain,
                        ParseError::new(
                            token.index(),
                            ParseErrorKind::InvalidExpressionInParentheses,
                        ),
                    ));
                }
            }
            _ => {
                new_errors.push(ParseError::new(
                    token.index(),
                    ParseErrorKind::UnexpectedToken,
                ));
            }
        }
    }

    errors.extend(new_errors);
    Err((
        remain,
        ParseError::new(paren_open_index, ParseErrorKind::ParenthesesIsNotClosed),
    ))
}

fn parse_pure_number(tokens: &[Token]) -> ParseResult<'_, Number> {
    match tokens.first().map(|t| (t.index(), t.kind())) {
        Some((i, TokenKind::Number(n))) => Ok((tokens.split_at(1).1, i, n.clone())),
        Some((i, _)) => Err((
            tokens,
            ParseError::new(i, ParseErrorKind::ExpectNumberButOthers),
        )),
        _ => Err((
            tokens,
            ParseError::new(0, ParseErrorKind::ExpectNumberButEmpty),
        )),
    }
}

fn parse_signed_number(tokens: &[Token]) -> ParseResult<'_, Number> {
    let mut remain = tokens;

    let (sign_index, is_negative) = match remain.first().map(|t| (t.index(), t.kind())) {
        Some((i, TokenKind::Operator(Operator::Add))) => {
            remain = remain.split_at(1).1;
            (Some(i), false)
        }
        Some((i, TokenKind::Operator(Operator::Sub))) => {
            remain = remain.split_at(1).1;
            (Some(i), true)
        }
        _ => (None, false),
    };

    let (remain, number_index, raw_num) = parse_pure_number(remain)?;

    let num = if is_negative {
        match raw_num {
            Number::Float(v) => Number::Float(v.neg()),
            Number::Int(v) => {
                if let Some(n) = v.checked_neg() {
                    Number::Int(n)
                } else {
                    return Err((
                        remain,
                        ParseError::new(number_index, ParseErrorKind::TooSmallNegativeInt),
                    ));
                }
            }
        }
    } else {
        raw_num.clone()
    };

    Ok((remain, sign_index.unwrap_or(number_index), num))
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let result = parse(&case_01).unwrap();
            assert_eq!(
                result,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Add
                ))
            );

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Mul)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
                Token::new(4, TokenKind::Operator(Operator::Sub)),
                Token::new(5, TokenKind::Number(Number::Int(4))),
            ];
            let result = parse(&case_02).unwrap();
            assert_eq!(
                result,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Number(Number::Int(2))),
                        Box::new(Expression::Number(Number::Int(3))),
                        Operator::Mul
                    ))),
                    Box::new(Expression::Number(Number::Int(4))),
                    Operator::Sub
                ))
            );

            let case_03 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
                Token::new(4, TokenKind::Operator(Operator::Sub)),
                Token::new(5, TokenKind::Number(Number::Int(4))),
            ];
            let result = parse(&case_03).unwrap();
            assert_eq!(
                result,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Number(Number::Int(2))),
                        Box::new(Expression::Number(Number::Int(3))),
                        Operator::Add
                    ))),
                    Box::new(Expression::Number(Number::Int(4))),
                    Operator::Sub
                ))
            );

            let case_04 = [
                Token::new(0, TokenKind::Number(Number::Float(1.2))),
                Token::new(1, TokenKind::Operator(Operator::Div)),
                Token::new(2, TokenKind::ParenthesesOpen),
                Token::new(3, TokenKind::Number(Number::Float(3.4))),
                Token::new(4, TokenKind::Operator(Operator::Add)),
                Token::new(5, TokenKind::Number(Number::Float(5.6))),
                Token::new(6, TokenKind::ParenthesesClose),
            ];
            let result = parse(&case_04).unwrap();
            assert_eq!(
                result,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Float(1.2))),
                    Box::new(Expression::Parentheses(Box::new(Expression::Operation(
                        Operation::new(
                            Box::new(Expression::Number(Number::Float(3.4))),
                            Box::new(Expression::Number(Number::Float(5.6))),
                            Operator::Add
                        )
                    )))),
                    Operator::Div
                ))
            );

            let case_05 = [
                Token::new(0, TokenKind::Number(Number::Float(1.2))),
                Token::new(1, TokenKind::Operator(Operator::Add)),
                Token::new(2, TokenKind::Number(Number::Int(3))),
                Token::new(3, TokenKind::Operator(Operator::Rem)),
                Token::new(4, TokenKind::Number(Number::Int(4))),
                Token::new(5, TokenKind::Operator(Operator::Sub)),
                Token::new(6, TokenKind::Number(Number::Float(5.6))),
            ];
            let result = parse(&case_05).unwrap();
            assert_eq!(
                result,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Number(Number::Float(1.2))),
                        Box::new(Expression::Operation(Operation::new(
                            Box::new(Expression::Number(Number::Int(3))),
                            Box::new(Expression::Number(Number::Int(4))),
                            Operator::Rem
                        ))),
                        Operator::Add,
                    ))),
                    Box::new(Expression::Number(Number::Float(5.6))),
                    Operator::Sub
                ))
            );
        }

        #[test]
        fn failed_when_invalid_tokens() {
            let case_01 = [];
            let errors = parse(&case_01).err().unwrap();
            assert_eq!(
                errors,
                vec![ParseError::new(0, ParseErrorKind::ExpectNumberButEmpty)]
            );

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(2))),
                Token::new(4, TokenKind::Number(Number::Int(3))),
                Token::new(5, TokenKind::Operator(Operator::Sub)),
                Token::new(6, TokenKind::Number(Number::Int(4))),
            ];
            let errors = parse(&case_02).err().unwrap();
            assert_eq!(
                errors,
                vec![
                    ParseError::new(4, ParseErrorKind::UnexpectedToken),
                    ParseError::new(5, ParseErrorKind::UnexpectedToken),
                    ParseError::new(6, ParseErrorKind::UnexpectedToken),
                ]
            );
        }
    }

    mod parse_operation_add_sub {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_operation_add_sub(&case_01, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Add
                ))
            );

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Sub)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_operation_add_sub(&case_02, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Sub
                ))
            );

            let case_03 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
                Token::new(4, TokenKind::Operator(Operator::Add)),
                Token::new(5, TokenKind::Number(Number::Int(4))),
                Token::new(6, TokenKind::Operator(Operator::Add)),
                Token::new(7, TokenKind::Number(Number::Int(5))),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_operation_add_sub(&case_03, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Operation(Operation::new(
                            Box::new(Expression::Number(Number::Int(2))),
                            Box::new(Expression::Number(Number::Int(3))),
                            Operator::Add
                        ))),
                        Box::new(Expression::Number(Number::Int(4))),
                        Operator::Add
                    ))),
                    Box::new(Expression::Number(Number::Int(5))),
                    Operator::Add
                ))
            );
        }

        #[test]
        fn failed_when_invalid_tokens() {
            let case_01 = [
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_operation_add_sub(&case_01, &mut errors)
                .err()
                .unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(
                error,
                ParseError::new(0, ParseErrorKind::ExpectNumberButEmpty)
            );

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Operator(Operator::Mul)),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_operation_add_sub(&case_02, &mut errors)
                .err()
                .unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[Token::new(3, TokenKind::Operator(Operator::Mul))]);
            assert_eq!(
                error,
                ParseError::new(3, ParseErrorKind::ExpectNumberButOthers)
            );
        }
    }

    mod parse_operation_mu_div_rem {

        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Mul)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let mut errors = vec![];
            let (remain, index, value) =
                parse_operation_mul_div_rem(&case_01, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Mul
                ))
            );

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Div)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let mut errors = vec![];
            let (remain, index, value) =
                parse_operation_mul_div_rem(&case_02, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Div
                ))
            );

            let case_03 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Rem)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
            ];
            let mut errors = vec![];
            let (remain, index, value) =
                parse_operation_mul_div_rem(&case_03, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Number(Number::Int(3))),
                    Operator::Rem
                ))
            );

            let case_04 = [
                Token::new(1, TokenKind::Number(Number::Int(2))),
                Token::new(2, TokenKind::Operator(Operator::Rem)),
                Token::new(3, TokenKind::Number(Number::Int(3))),
                Token::new(4, TokenKind::Operator(Operator::Mul)),
                Token::new(5, TokenKind::Number(Number::Int(4))),
                Token::new(6, TokenKind::Operator(Operator::Div)),
                Token::new(7, TokenKind::Number(Number::Int(5))),
            ];
            let mut errors = vec![];
            let (remain, index, value) =
                parse_operation_mul_div_rem(&case_04, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Operation(Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Operation(Operation::new(
                            Box::new(Expression::Number(Number::Int(2))),
                            Box::new(Expression::Number(Number::Int(3))),
                            Operator::Rem
                        ))),
                        Box::new(Expression::Number(Number::Int(4))),
                        Operator::Mul
                    ))),
                    Box::new(Expression::Number(Number::Int(5))),
                    Operator::Div
                ))
            );
        }
    }

    mod parse_parentheses {
        use super::*;

        #[test]
        fn success_when_invalid_tokens() {
            let case_01 = [
                Token::new(1, TokenKind::ParenthesesOpen),
                Token::new(2, TokenKind::Number(Number::Int(1))),
                Token::new(3, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_01, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Number(Number::Int(1))))
            );

            let case_02 = [
                Token::new(1, TokenKind::ParenthesesOpen),
                Token::new(2, TokenKind::Operator(Operator::Sub)),
                Token::new(3, TokenKind::Number(Number::Int(1))),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_02, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Number(Number::Int(-1))))
            );

            let case_03 = [
                Token::new(1, TokenKind::ParenthesesOpen),
                Token::new(2, TokenKind::Number(Number::Int(1))),
                Token::new(3, TokenKind::Operator(Operator::Add)),
                Token::new(4, TokenKind::Number(Number::Int(4))),
                Token::new(5, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_03, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(4))),
                    Operator::Add
                ))))
            );

            let case_04 = [
                Token::new(1, TokenKind::ParenthesesOpen),
                Token::new(2, TokenKind::Number(Number::Int(1))),
                Token::new(3, TokenKind::Operator(Operator::Mul)),
                Token::new(4, TokenKind::Number(Number::Int(4))),
                Token::new(5, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_04, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(4))),
                    Operator::Mul
                ))))
            );

            let case_05 = [
                Token::new(1, TokenKind::ParenthesesOpen),
                Token::new(2, TokenKind::Number(Number::Int(1))),
                Token::new(3, TokenKind::Operator(Operator::Mul)),
                Token::new(4, TokenKind::Number(Number::Int(4))),
                Token::new(5, TokenKind::ParenthesesClose),
                Token::new(6, TokenKind::Operator(Operator::Add)),
                Token::new(7, TokenKind::Number(Number::Int(7))),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_05, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(
                remain,
                &[
                    Token::new(6, TokenKind::Operator(Operator::Add)),
                    Token::new(7, TokenKind::Number(Number::Int(7))),
                ]
            );
            assert_eq!(index, 1);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(4))),
                    Operator::Mul
                ))))
            );

            let case_06 = [
                Token::new(1, TokenKind::Operator(Operator::Sub)),
                Token::new(2, TokenKind::Number(Number::Int(1))),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_06, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 1);
            assert_eq!(value, Expression::Number(Number::Int(-1)));

            let case_07 = [
                Token::new(2, TokenKind::ParenthesesOpen),
                Token::new(3, TokenKind::Number(Number::Int(2))),
                Token::new(4, TokenKind::Operator(Operator::Mul)),
                Token::new(5, TokenKind::ParenthesesOpen),
                Token::new(6, TokenKind::Number(Number::Int(3))),
                Token::new(7, TokenKind::Operator(Operator::Add)),
                Token::new(8, TokenKind::Number(Number::Int(4))),
                Token::new(9, TokenKind::ParenthesesClose),
                Token::new(10, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, index, value) = parse_parentheses(&case_07, &mut errors).unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(index, 2);
            assert_eq!(
                value,
                Expression::Parentheses(Box::new(Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(2))),
                    Box::new(Expression::Parentheses(Box::new(Expression::Operation(
                        Operation::new(
                            Box::new(Expression::Number(Number::Int(3))),
                            Box::new(Expression::Number(Number::Int(4))),
                            Operator::Add
                        )
                    )))),
                    Operator::Mul,
                ))))
            );
        }

        #[test]
        fn failed_when_invalid_tokens() {
            let case_01 = [
                Token::new(1, TokenKind::Operator(Operator::Mul)),
                Token::new(2, TokenKind::Number(Number::Int(1))),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_parentheses(&case_01, &mut errors).err().unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(
                remain,
                &[
                    Token::new(1, TokenKind::Operator(Operator::Mul)),
                    Token::new(2, TokenKind::Number(Number::Int(1))),
                ]
            );
            assert_eq!(
                error,
                ParseError::new(1, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_02 = [
                Token::new(0, TokenKind::ParenthesesOpen),
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Operator(Operator::Add)),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_parentheses(&case_02, &mut errors).err().unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(
                error,
                ParseError::new(4, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_03 = [
                Token::new(0, TokenKind::ParenthesesOpen),
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Operator(Operator::Mul)),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_parentheses(&case_03, &mut errors).err().unwrap();
            assert_eq!(
                errors,
                vec![ParseError::new(3, ParseErrorKind::UnexpectedToken),]
            );
            assert_eq!(remain, &[]);
            assert_eq!(
                error,
                ParseError::new(4, ParseErrorKind::InvalidExpressionInParentheses)
            );

            let case_04 = [
                Token::new(0, TokenKind::ParenthesesOpen),
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Operator(Operator::Mul)),
                Token::new(4, TokenKind::Operator(Operator::Mul)),
                Token::new(5, TokenKind::ParenthesesClose),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_parentheses(&case_04, &mut errors).err().unwrap();
            assert_eq!(
                errors,
                vec![
                    ParseError::new(3, ParseErrorKind::UnexpectedToken),
                    ParseError::new(4, ParseErrorKind::UnexpectedToken)
                ]
            );
            assert_eq!(remain, &[]);
            assert_eq!(
                error,
                ParseError::new(5, ParseErrorKind::InvalidExpressionInParentheses)
            );

            let case_05 = [
                Token::new(0, TokenKind::ParenthesesOpen),
                Token::new(1, TokenKind::Number(Number::Int(1))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(2))),
            ];
            let mut errors = vec![];
            let (remain, error) = parse_parentheses(&case_05, &mut errors).err().unwrap();
            assert_eq!(errors, vec![]);
            assert_eq!(remain, &[]);
            assert_eq!(
                error,
                ParseError::new(0, ParseErrorKind::ParenthesesIsNotClosed)
            );
        }
    }

    mod parse_pure_number {
        use super::*;

        #[test]
        fn success_when_valid_tokens() {
            let case_01 = [Token::new(0, TokenKind::Number(Number::Float(0.12)))];
            let (remain, index, value) = parse_pure_number(&case_01).unwrap();
            assert_eq!(remain, &[]);
            assert_eq!(index, 0);
            assert_eq!(value, Number::Float(0.12));

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(3))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(4))),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let (remain, index, value) = parse_pure_number(&case_02).unwrap();
            assert_eq!(
                remain,
                &[
                    Token::new(2, TokenKind::Operator(Operator::Add)),
                    Token::new(3, TokenKind::Number(Number::Int(4))),
                    Token::new(4, TokenKind::ParenthesesClose),
                ]
            );
            assert_eq!(index, 1);
            assert_eq!(value, Number::Int(3));
        }

        #[test]
        fn failed_when_invalid_tokens() {
            let case_01 = [Token::new(0, TokenKind::Operator(Operator::Add))];
            let (remain, err) = parse_pure_number(&case_01).err().unwrap();
            assert_eq!(remain, &case_01);
            assert_eq!(
                err,
                ParseError::new(0, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_02 = [
                Token::new(1, TokenKind::Operator(Operator::Add)),
                Token::new(2, TokenKind::Number(Number::Int(1))),
                Token::new(3, TokenKind::ParenthesesClose),
            ];
            let (remain, err) = parse_pure_number(&case_02).err().unwrap();
            assert_eq!(remain, &case_02);
            assert_eq!(
                err,
                ParseError::new(1, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_03 = [];
            let (remain, err) = parse_pure_number(&case_03).err().unwrap();
            assert_eq!(remain, &case_03);
            assert_eq!(
                err,
                ParseError::new(0, ParseErrorKind::ExpectNumberButEmpty)
            );
        }
    }

    mod parse_signed_number {
        use super::*;

        #[test]
        fn success_when_valid_tokens() {
            let case_01 = [Token::new(0, TokenKind::Number(Number::Float(0.12)))];
            let (remain, index, value) = parse_signed_number(&case_01).unwrap();
            assert_eq!(remain, &[]);
            assert_eq!(index, 0);
            assert_eq!(value, Number::Float(0.12));

            let case_02 = [
                Token::new(1, TokenKind::Number(Number::Int(3))),
                Token::new(2, TokenKind::Operator(Operator::Add)),
                Token::new(3, TokenKind::Number(Number::Int(4))),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let (remain, index, value) = parse_signed_number(&case_02).unwrap();
            assert_eq!(
                remain,
                &[
                    Token::new(2, TokenKind::Operator(Operator::Add)),
                    Token::new(3, TokenKind::Number(Number::Int(4))),
                    Token::new(4, TokenKind::ParenthesesClose),
                ]
            );
            assert_eq!(index, 1);
            assert_eq!(value, Number::Int(3));

            let case_03 = [
                Token::new(0, TokenKind::Operator(Operator::Add)),
                Token::new(1, TokenKind::Number(Number::Float(0.12))),
            ];
            let (remain, index, value) = parse_signed_number(&case_03).unwrap();
            assert_eq!(remain, &[]);
            assert_eq!(index, 0);
            assert_eq!(value, Number::Float(0.12));

            let case_04 = [
                Token::new(1, TokenKind::Operator(Operator::Sub)),
                Token::new(2, TokenKind::Number(Number::Int(3))),
                Token::new(3, TokenKind::ParenthesesClose),
            ];
            let (remain, index, value) = parse_signed_number(&case_04).unwrap();
            assert_eq!(remain, &[Token::new(3, TokenKind::ParenthesesClose),]);
            assert_eq!(index, 1);
            assert_eq!(value, Number::Int(-3));
        }

        #[test]
        fn failed_when_invalid_token() {
            let case_01 = [Token::new(1, TokenKind::Operator(Operator::Mul))];
            let (remain, err) = parse_signed_number(&case_01).err().unwrap();
            assert_eq!(remain, &case_01);
            assert_eq!(
                err,
                ParseError::new(1, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_02 = [
                Token::new(2, TokenKind::ParenthesesOpen),
                Token::new(3, TokenKind::Number(Number::Int(2))),
                Token::new(4, TokenKind::ParenthesesClose),
            ];
            let (remain, err) = parse_signed_number(&case_02).err().unwrap();
            assert_eq!(remain, &case_02);
            assert_eq!(
                err,
                ParseError::new(2, ParseErrorKind::ExpectNumberButOthers)
            );

            let case_03 = [];
            let (remain, err) = parse_signed_number(&case_03).err().unwrap();
            assert_eq!(remain, &case_03);
            assert_eq!(
                err,
                ParseError::new(0, ParseErrorKind::ExpectNumberButEmpty)
            );
        }
    }
}
