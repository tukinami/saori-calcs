use std::fmt::Display;

use self::{
    ast::{CalcMode, Eval, Number},
    parse::parse,
    preprocess::preprocess,
    tokenize::tokenize,
};

mod ast;
mod parse;
mod preprocess;
mod tokenize;

const SEPARATOR: &str = "|";

pub(crate) fn calc(calc_mode_str: &str, expression_str: &str) -> Result<String, String> {
    let calc_mode = if let Ok(mode) = calc_mode_str.parse::<CalcMode>() {
        mode
    } else {
        return Err(format!(
            "モード指定エラー: 指定のモード文字列ではありません 文字列:'{}'",
            calc_mode_str
        ));
    };

    let preprocessed_string = preprocess(expression_str);

    let tokens = match tokenize(&preprocessed_string) {
        Ok(tokens) => tokens,
        Err(errors) => {
            return Err(to_string_errors(&errors));
        }
    };

    let expr = match parse(&tokens) {
        Ok(expr) => expr,
        Err(errors) => {
            return Err(to_string_errors(&errors));
        }
    };

    let mut errors = Vec::new();
    match expr.eval(&calc_mode, &mut errors) {
        Ok(num) => {
            let num = calc_mode.to_int_if_needed_on_after(&num);
            let str = match num {
                Number::Int(i) if i >= i32::MAX as i128 => i32::MAX.to_string(),
                Number::Int(i) if i <= i32::MIN as i128 => i32::MIN.to_string(),
                Number::Int(i) => i.to_string(),
                Number::Float(f) => f.to_string(),
            };

            Ok(str)
        }
        Err(()) => Err(to_string_errors(&errors)),
    }
}

fn to_string_errors<T>(errors: &[T]) -> String
where
    T: Display,
{
    errors
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<String>>()
        .join(SEPARATOR)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod calc {
        use super::*;

        #[test]
        fn success_when_valid_str_with_multibyte_char() {
            let calc_mode_str = "before_floor";

            let case_01 = "１２３４５６７８９０";
            assert_eq!(calc(calc_mode_str, case_01), Ok("1234567890".to_string()));

            let case_02 = "（1＋2）−（3×4）÷(5％6)";
            assert_eq!(calc(calc_mode_str, case_02), Ok("1".to_string()));

            let case_03 = "（4／2＊3）";
            assert_eq!(calc(calc_mode_str, case_03), Ok("6".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_max_value() {
            let calc_mode_str = "";

            let case_01 = "2147483647+10";
            assert_eq!(calc(calc_mode_str, case_01), Ok("2147483647".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_min_value() {
            let calc_mode_str = "";

            let case_01 = "-2147483648-100";
            assert_eq!(calc(calc_mode_str, case_01), Ok("-2147483648".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_before_floor() {
            let calc_mode_str = "before_floor";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("5".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("2".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_after_floor() {
            let calc_mode_str = "after_floor";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("5".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("4".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_before_ceil() {
            let calc_mode_str = "before_ceil";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("6".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("8".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_after_cail() {
            let calc_mode_str = "after_ceil";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("6".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("5".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_before_round() {
            let calc_mode_str = "before_round";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("6".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("6".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_after_round() {
            let calc_mode_str = "after_round";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("6".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("5".to_string()));
        }

        #[test]
        fn success_when_valid_str_with_float() {
            let calc_mode_str = "";

            let case_01 = "3";
            assert_eq!(calc(calc_mode_str, case_01), Ok("3".to_string()));

            let case_02 = "3.0 + 2.5";
            assert_eq!(calc(calc_mode_str, case_02), Ok("5.5".to_string()));

            let case_03 = "1.5 * (1.2 + 1.8)";
            assert_eq!(calc(calc_mode_str, case_03), Ok("4.5".to_string()));
        }

        #[test]
        fn failed_when_invalid_str() {
            let case_01 = "（3 / 0）";
            assert!(calc("", case_01).is_err());

            let case_02 = format!("{}*{}", i128::MAX, i128::MAX);
            assert!(calc("", &case_02).is_err());

            let case_03 = "1.0%2.0";
            assert!(calc("", &case_03).is_err());
        }
    }
}
