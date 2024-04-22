use std::fmt::Display;

use super::ast::{Number, Operator};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TokenizeError {
    index: usize,
    kind: TokenizeErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenizeErrorKind {
    InvalidToken,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    index: usize,
    kind: TokenKind,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind {
    ParenthesesOpen,
    ParenthesesClose,
    Number(Number),
    Operator(Operator),
}

impl TokenizeError {
    pub fn new(index: usize, kind: TokenizeErrorKind) -> TokenizeError {
        TokenizeError { index, kind }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn kind(&self) -> &TokenizeErrorKind {
        &self.kind
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = "字句解析エラー: ";
        let index = self.index() + 1;

        match self.kind() {
            TokenizeErrorKind::InvalidToken => {
                write!(f, "{}解析できない文字があります {}文字目", tag, index)
            }
        }
    }
}

impl Token {
    pub fn new(index: usize, kind: TokenKind) -> Token {
        Token { index, kind }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

pub(crate) fn tokenize(source: &str) -> Result<Vec<Token>, Vec<TokenizeError>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut i = 0;
    let mut s = source;

    while !s.is_empty() {
        if let Some(mid) = find_whitespace(s) {
            s = s.split_at(mid).1;
            i += mid;
        }

        match tokenize_token(s) {
            Ok((remain, range_index, kind)) => {
                s = remain;
                tokens.push(Token::new(i, kind));
                i += range_index;
            }
            Err(remain) => {
                errors.push(TokenizeError::new(i, TokenizeErrorKind::InvalidToken));
                let range_index = next_char_index(remain);
                s = remain.split_at(range_index).1;
                i += range_index;
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(tokens)
    }
}

fn tokenize_token(source: &str) -> Result<(&str, usize, TokenKind), &str> {
    tokenize_parentheses_open(source)
        .or(tokenize_parentheses_close(source))
        .or(tokenize_operator_add(source))
        .or(tokenize_operator_sub(source))
        .or(tokenize_operator_mul(source))
        .or(tokenize_operator_div(source))
        .or(tokenize_operator_rem(source))
        .or(tokenize_number(source))
}

macro_rules! tokenizer_with_single_char {
    ($name:ident, $func:expr, $kind:expr) => {
        fn $name(source: &str) -> Result<(&str, usize, TokenKind), &str> {
            find_starts_with_a(source, $func).ok_or(source).map(|mid| {
                let (_target, remain) = source.split_at(mid);
                (remain, mid, $kind)
            })
        }
    };
}

tokenizer_with_single_char!(
    tokenize_parentheses_open,
    term_parentheses_open,
    TokenKind::ParenthesesOpen
);

tokenizer_with_single_char!(
    tokenize_parentheses_close,
    term_parentheses_close,
    TokenKind::ParenthesesClose
);

tokenizer_with_single_char!(
    tokenize_operator_add,
    term_plus,
    TokenKind::Operator(Operator::Add)
);

tokenizer_with_single_char!(
    tokenize_operator_sub,
    term_minus,
    TokenKind::Operator(Operator::Sub)
);

tokenizer_with_single_char!(
    tokenize_operator_mul,
    term_asterisk,
    TokenKind::Operator(Operator::Mul)
);

tokenizer_with_single_char!(
    tokenize_operator_div,
    term_slash,
    TokenKind::Operator(Operator::Div)
);

tokenizer_with_single_char!(
    tokenize_operator_rem,
    term_percent,
    TokenKind::Operator(Operator::Rem)
);

fn tokenize_number(source: &str) -> Result<(&str, usize, TokenKind), &str> {
    find_float(source)
        .or(find_digit(source))
        .ok_or(source)
        .and_then(|mid| {
            let (target, remain) = source.split_at(mid);

            target
                .parse::<i128>()
                .map_err(|_| source)
                .map(Number::Int)
                .or(target.parse::<f64>().map_err(|_| source).map(Number::Float))
                .map(|v| (remain, mid, TokenKind::Number(v)))
        })
}

fn find_float(source: &str) -> Option<usize> {
    find_digit(source)
        .map(|i_integer_part| (source.split_at(i_integer_part).1, i_integer_part))
        .and_then(|(remain, i_integer_part)| {
            find_starts_with_a(remain, term_period)
                .map(|i_period| (remain.split_at(i_period).1, i_integer_part + i_period))
        })
        .and_then(|(remain, i)| find_digit(remain).map(|i_float_part| i + i_float_part))
}

fn find_digit(source: &str) -> Option<usize> {
    find_while(source, term_digit)
}

fn find_whitespace(source: &str) -> Option<usize> {
    find_while(source, term_whitespace)
}

fn find_while(source: &str, func: fn(char) -> bool) -> Option<usize> {
    if source.starts_with(func) {
        Some(source.find(|c: char| !func(c)).unwrap_or(source.len()))
    } else {
        None
    }
}

fn find_starts_with_a(source: &str, func: fn(char) -> bool) -> Option<usize> {
    if source.starts_with(func) {
        Some(next_char_index(source))
    } else {
        None
    }
}

fn next_char_index(source: &str) -> usize {
    if source.is_empty() {
        return 0;
    }

    let mut index = 1;

    while !source.is_char_boundary(index) {
        index += 1;
    }

    index
}

fn term_whitespace(c: char) -> bool {
    c.is_whitespace()
}

fn term_digit(c: char) -> bool {
    c.is_ascii_digit()
}

fn term_period(c: char) -> bool {
    c == '.'
}

fn term_plus(c: char) -> bool {
    c == '+'
}

fn term_minus(c: char) -> bool {
    c == '-'
}

fn term_asterisk(c: char) -> bool {
    c == '*'
}

fn term_slash(c: char) -> bool {
    c == '/'
}

fn term_percent(c: char) -> bool {
    c == '%'
}

fn term_parentheses_open(c: char) -> bool {
    c == '('
}

fn term_parentheses_close(c: char) -> bool {
    c == ')'
}

#[cfg(test)]
mod tests {
    use super::*;

    mod tokenize {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "1";
            assert_eq!(
                tokenize(case_01),
                Ok(vec![Token::new(0, TokenKind::Number(Number::Int(1)))])
            );

            let case_02 = "1+2.3";
            assert_eq!(
                tokenize(case_02),
                Ok(vec![
                    Token::new(0, TokenKind::Number(Number::Int(1))),
                    Token::new(1, TokenKind::Operator(Operator::Add)),
                    Token::new(2, TokenKind::Number(Number::Float(2.3))),
                ])
            );

            let case_03 = "";
            assert_eq!(tokenize(case_03), Ok(vec![]));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "あ+1";
            assert_eq!(
                tokenize(case_01),
                Err(vec![TokenizeError::new(0, TokenizeErrorKind::InvalidToken)])
            );

            let case_02 = "1+あ";
            assert_eq!(
                tokenize(case_02),
                Err(vec![TokenizeError::new(2, TokenizeErrorKind::InvalidToken)])
            );

            let case_03 = "1あ1";
            assert_eq!(
                tokenize(case_03),
                Err(vec![TokenizeError::new(1, TokenizeErrorKind::InvalidToken)])
            );

            let case_04 = "1ああa1";
            assert_eq!(
                tokenize(case_04),
                Err(vec![
                    TokenizeError::new(1, TokenizeErrorKind::InvalidToken),
                    TokenizeError::new(4, TokenizeErrorKind::InvalidToken),
                    TokenizeError::new(7, TokenizeErrorKind::InvalidToken),
                ])
            );
        }
    }

    mod tokenize_token {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let cases = ["(", ")", "+", "-", "*", "/", "%", "2", "3.1"];

            for case in cases {
                assert!(tokenize_token(case).is_ok());
            }
        }

        #[test]
        fn failed_when_invalid_str() {
            let cases = ["あ", "a", " ", "  1"];

            for case in cases {
                assert!(tokenize_token(case).is_err());
            }
        }
    }

    mod tokenize_parentheses_open {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "(";
            let (remain, end_index, kind) = tokenize_parentheses_open(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::ParenthesesOpen);

            let case_02 = "(2";
            let (remain, end_index, kind) = tokenize_parentheses_open(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::ParenthesesOpen);
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_parentheses_open(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_parentheses_open(case_02), Err(case_02));
        }
    }

    mod tokenize_parentheses_close {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = ")";
            let (remain, end_index, kind) = tokenize_parentheses_close(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::ParenthesesClose);

            let case_02 = ")2";
            let (remain, end_index, kind) = tokenize_parentheses_close(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::ParenthesesClose);
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_parentheses_close(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_parentheses_close(case_02), Err(case_02));
        }
    }

    mod tokenize_operator_add {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "+";
            let (remain, end_index, kind) = tokenize_operator_add(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Add));

            let case_02 = "+2";
            let (remain, end_index, kind) = tokenize_operator_add(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Add));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_operator_add(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_operator_add(case_02), Err(case_02));
        }
    }

    mod tokenize_operator_sub {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "-";
            let (remain, end_index, kind) = tokenize_operator_sub(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Sub));

            let case_02 = "-2";
            let (remain, end_index, kind) = tokenize_operator_sub(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Sub));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_operator_sub(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_operator_sub(case_02), Err(case_02));
        }
    }

    mod tokenize_operator_mul {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "*";
            let (remain, end_index, kind) = tokenize_operator_mul(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Mul));

            let case_02 = "*2";
            let (remain, end_index, kind) = tokenize_operator_mul(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Mul));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_operator_mul(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_operator_mul(case_02), Err(case_02));
        }
    }

    mod tokenize_operator_div {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "/";
            let (remain, end_index, kind) = tokenize_operator_div(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Div));

            let case_02 = "/2";
            let (remain, end_index, kind) = tokenize_operator_div(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Div));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_operator_div(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_operator_div(case_02), Err(case_02));
        }
    }

    mod tokenize_operator_rem {
        use super::*;

        #[test]
        fn success_when_valid_str() {
            let case_01 = "%";
            let (remain, end_index, kind) = tokenize_operator_rem(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Rem));

            let case_02 = "%2";
            let (remain, end_index, kind) = tokenize_operator_rem(case_02).unwrap();
            assert_eq!(remain, "2");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Operator(Operator::Rem));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "1";
            assert_eq!(tokenize_operator_rem(case_01), Err(case_01));

            let case_02 = "";
            assert_eq!(tokenize_operator_rem(case_02), Err(case_02));
        }
    }

    mod tokenize_number {
        use super::*;

        #[test]
        fn success_when_valid_number_without_sign() {
            let case_01 = "1";
            let (remain, end_index, kind) = tokenize_number(case_01).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Number(Number::Int(1)));

            let case_02 = "0.12";
            let (remain, end_index, kind) = tokenize_number(case_02).unwrap();
            assert_eq!(remain, "");
            assert_eq!(end_index, 4);
            assert_eq!(kind, TokenKind::Number(Number::Float(0.12)));

            let case_03 = "1+";
            let (remain, end_index, kind) = tokenize_number(case_03).unwrap();
            assert_eq!(remain, "+");
            assert_eq!(end_index, 1);
            assert_eq!(kind, TokenKind::Number(Number::Int(1)));

            let case_04 = "0.12-";
            let (remain, end_index, kind) = tokenize_number(case_04).unwrap();
            assert_eq!(remain, "-");
            assert_eq!(end_index, 4);
            assert_eq!(kind, TokenKind::Number(Number::Float(0.12)));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "+0.5";
            assert_eq!(tokenize_number(case_01), Err(case_01));

            let case_02 = "a";
            assert_eq!(tokenize_number(case_02), Err(case_02));

            let case_03 = "";
            assert_eq!(tokenize_number(case_03), Err(case_03));
        }
    }

    mod find_float {
        use super::*;

        #[test]
        fn some_value_when_it_starts_with_float() {
            let case_01 = "0.1234ffff";
            assert_eq!(find_float(case_01), Some(6));
            let (first, last) = case_01.split_at(6);
            assert_eq!(first, "0.1234");
            assert_eq!(last, "ffff");

            let case_02 = "1234.5ffff";
            assert_eq!(find_float(case_02), Some(6));

            let case_03 = "0.12ffff34.56";
            assert_eq!(find_float(case_03), Some(4));

            let case_04 = "0.1";
            assert_eq!(find_float(case_04), Some(3));
        }

        #[test]
        fn none_when_it_does_not_starts_with_float() {
            let case_01 = "f0.1234ffff";
            assert_eq!(find_float(case_01), None);

            let case_02 = "f1234.5ffff";
            assert_eq!(find_float(case_02), None);

            let case_03 = "f0.12ffff34.56";
            assert_eq!(find_float(case_03), None);

            let case_04 = "123";
            assert_eq!(find_float(case_04), None);
        }
    }

    mod find_digit {

        use super::*;

        #[test]
        fn some_value_when_it_starts_with_digit() {
            let case_01 = "1234ffff";
            assert_eq!(find_digit(case_01), Some(4));
            let (first, last) = case_01.split_at(4);
            assert_eq!(first, "1234");
            assert_eq!(last, "ffff");

            let case_02 = "1ffff";
            assert_eq!(find_digit(case_02), Some(1));

            let case_03 = "1234ffff1234ffff";
            assert_eq!(find_digit(case_03), Some(4));

            let case_04 = "1";
            assert_eq!(find_digit(case_04), Some(1));
        }

        #[test]
        fn none_when_it_does_not_start_with_digit() {
            let case_01 = "f1234ffff";
            assert_eq!(find_digit(case_01), None);

            let case_02 = "fff1ffff";
            assert_eq!(find_digit(case_02), None);

            let case_03 = "f1234ffff1234ffff";
            assert_eq!(find_digit(case_03), None);
        }
    }

    mod find_whitespace {
        use super::*;

        #[test]
        fn some_value_when_it_starts_with_whitespaces() {
            let case_01 = " ";
            assert_eq!(find_whitespace(case_01), Some(1));

            let case_02 = "\t";
            assert_eq!(find_whitespace(case_02), Some(1));

            let case_03 = "  \t ";
            assert_eq!(find_whitespace(case_03), Some(4));
        }

        #[test]
        fn none_when_it_does_not_start_with_whitespace() {
            let case_01 = "a";
            assert_eq!(find_whitespace(case_01), None);

            let case_02 = "a ";
            assert_eq!(find_whitespace(case_02), None);
        }
    }

    mod find_while {
        use super::*;

        #[test]
        fn some_value_when_it_starts_with_term() {
            fn func(c: char) -> bool {
                c.is_ascii_digit()
            }

            let case_01 = "1234ffff";
            assert_eq!(find_while(case_01, func), Some(4));

            let case_02 = "1ffff";
            assert_eq!(find_while(case_02, func), Some(1));

            let case_03 = "1234ffff1234ffff";
            assert_eq!(find_while(case_03, func), Some(4));

            let case_04 = "1234";
            assert_eq!(find_while(case_04, func), Some(4));
        }

        #[test]
        fn none_when_it_does_not_start_with_term() {
            fn func(c: char) -> bool {
                c.is_ascii_digit()
            }

            let case_01 = "f1234ffff";
            assert_eq!(find_while(case_01, func), None);

            let case_02 = "fff1ffff";
            assert_eq!(find_while(case_02, func), None);

            let case_03 = "f1234ffff1234ffff";
            assert_eq!(find_while(case_03, func), None);
        }
    }

    mod find_starts_with_a {
        use super::*;

        #[test]
        fn some_value_when_it_starts_with_multibyte_char() {
            fn func(c: char) -> bool {
                c == 'あ'
            }

            let case_01 = "あいうえお";
            assert_eq!(find_starts_with_a(case_01, func), Some(3));
            assert!(case_01.is_char_boundary(3));

            let case_02 = "あiueo";
            assert_eq!(find_starts_with_a(case_02, func), Some(3));
            assert!(case_01.is_char_boundary(3));
        }

        #[test]
        fn some_value_when_it_starts_with_singlebyte_char() {
            fn func(c: char) -> bool {
                c == 'a'
            }

            let case_01 = "aいうえお";
            assert_eq!(find_starts_with_a(case_01, func), Some(1));
            assert!(case_01.is_char_boundary(1));

            let case_02 = "aiueo";
            assert_eq!(find_starts_with_a(case_02, func), Some(1));
            assert!(case_02.is_char_boundary(1));
        }

        #[test]
        fn none_when_it_does_not_start_with_term() {
            fn func(c: char) -> bool {
                c == 'あ'
            }

            let case_01 = "aiueo";
            assert_eq!(find_starts_with_a(case_01, func), None);

            let case_02 = "oeuiあ";
            assert_eq!(find_starts_with_a(case_02, func), None);
        }
    }

    mod next_char_index {
        use super::*;

        #[test]
        fn checking_value() {
            let case_01 = "";
            assert_eq!(next_char_index(case_01), 0);

            let case_02 = "aa";
            assert_eq!(next_char_index(case_02), 1);

            let case_03 = "ああ";
            assert_eq!(next_char_index(case_03), 3);

            let case_04 = "あa";
            assert_eq!(next_char_index(case_04), 3);

            let case_05 = "aあ";
            assert_eq!(next_char_index(case_05), 1);
        }
    }
}
