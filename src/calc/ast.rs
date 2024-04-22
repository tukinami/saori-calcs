use std::{fmt::Display, str::FromStr};

pub(crate) type EvalResult = Result<Number, ()>;

pub(crate) trait Eval {
    fn eval(&self, calc_mode: &CalcMode, errors: &mut Vec<EvalError>) -> EvalResult;
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct EvalError {
    expression: Expression,
    kind: EvalErrorKind,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum EvalErrorKind {
    RemTargetIsNotInt,
    OverflowEvalResult,
}

enum OperandPair {
    Int(i128, i128),
    Float(f64, f64),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expression {
    Number(Number),
    Parentheses(Box<Expression>),
    Operation(Operation),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Operation {
    operand_1: Box<Expression>,
    operand_2: Box<Expression>,
    operator: Operator,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Number {
    Int(i128),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum CalcMode {
    BeforeFloor,
    AfterFloor,
    BeforeCeil,
    AfterCeil,
    BeforeRound,
    AfterRound,
    Float,
}

impl EvalError {
    pub fn new(expression: Expression, kind: EvalErrorKind) -> EvalError {
        EvalError { expression, kind }
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn kind(&self) -> &EvalErrorKind {
        &self.kind
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = "実行時エラー: ";
        match self.kind() {
            EvalErrorKind::OverflowEvalResult => {
                write!(
                    f,
                    "{}計算結果がオーバーフローしました 計算: {}",
                    tag,
                    self.expression()
                )
            }
            EvalErrorKind::RemTargetIsNotInt => {
                write!(
                    f,
                    "{}%は整数同士以外使用できません 計算: {}",
                    tag,
                    self.expression()
                )
            }
        }
    }
}

impl Eval for Expression {
    fn eval(&self, calc_mode: &CalcMode, errors: &mut Vec<EvalError>) -> EvalResult {
        match self {
            Expression::Number(num) => Ok(calc_mode.to_int_if_needed_on_before(num)),
            Expression::Parentheses(exp) => exp.eval(calc_mode, errors),
            Expression::Operation(op) => op.eval(calc_mode, errors),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Number(num) => num.fmt(f),
            Expression::Parentheses(exp) => exp.fmt(f),
            Expression::Operation(op) => op.fmt(f),
        }
    }
}

impl Operation {
    pub fn new(
        operand_1: Box<Expression>,
        operand_2: Box<Expression>,
        operator: Operator,
    ) -> Operation {
        Operation {
            operand_1,
            operand_2,
            operator,
        }
    }

    pub fn operand_1(&self) -> &Expression {
        &self.operand_1
    }

    pub fn operand_2(&self) -> &Expression {
        &self.operand_2
    }

    pub fn operator(&self) -> &Operator {
        &self.operator
    }
}

impl Eval for Operation {
    fn eval(&self, calc_mode: &CalcMode, errors: &mut Vec<EvalError>) -> EvalResult {
        let operand_1 =
            calc_mode.to_int_if_needed_on_before(&self.operand_1().eval(calc_mode, errors)?);
        let operand_2 =
            calc_mode.to_int_if_needed_on_before(&self.operand_2().eval(calc_mode, errors)?);

        let operand_pair = match (operand_1, operand_2) {
            (Number::Int(o_1), Number::Float(o_2)) => OperandPair::Float(o_1 as f64, o_2),
            (Number::Float(o_1), Number::Int(o_2)) => OperandPair::Float(o_1, o_2 as f64),
            (Number::Int(o_1), Number::Int(o_2)) => OperandPair::Int(o_1, o_2),
            (Number::Float(o_1), Number::Float(o_2)) => OperandPair::Float(o_1, o_2),
        };

        match operand_pair {
            OperandPair::Int(op_1, op_2) => {
                let operator = match self.operator() {
                    Operator::Add => i128::checked_add,
                    Operator::Sub => i128::checked_sub,
                    Operator::Mul => i128::checked_mul,
                    Operator::Div => i128::checked_div,
                    Operator::Rem => i128::checked_rem,
                };

                if let Some(num) = operator(op_1, op_2) {
                    Ok(Number::Int(num))
                } else {
                    errors.push(EvalError::new(
                        Expression::Operation(self.clone()),
                        EvalErrorKind::OverflowEvalResult,
                    ));
                    Err(())
                }
            }
            OperandPair::Float(op_1, op_2) => match self.operator() {
                Operator::Add => Ok(Number::Float(op_1 + op_2)),
                Operator::Sub => Ok(Number::Float(op_1 - op_2)),
                Operator::Mul => Ok(Number::Float(op_1 * op_2)),
                Operator::Div => Ok(Number::Float(op_1 / op_2)),
                Operator::Rem => {
                    errors.push(EvalError::new(
                        Expression::Operation(self.clone()),
                        EvalErrorKind::RemTargetIsNotInt,
                    ));
                    Err(())
                }
            },
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.operand_1(),
            self.operator(),
            self.operand_2()
        )
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Int(v) => write!(f, "{}(整数)", v),
            Number::Float(v) => write!(f, "{}(小数)", v),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Rem => "%",
        };
        write!(f, "{}", s)
    }
}

impl CalcMode {
    pub fn to_int_if_needed_on_before(&self, number: &Number) -> Number {
        match (self, number) {
            (Self::BeforeFloor, Number::Float(f)) => Number::Int(f.floor() as i128),
            (Self::BeforeCeil, Number::Float(f)) => Number::Int(f.ceil() as i128),
            (Self::BeforeRound, Number::Float(f)) => Number::Int(f.round() as i128),
            _ => number.clone(),
        }
    }

    pub fn to_int_if_needed_on_after(&self, number: &Number) -> Number {
        match (self, number) {
            (Self::AfterFloor, Number::Float(f)) => Number::Int(f.floor() as i128),
            (Self::AfterCeil, Number::Float(f)) => Number::Int(f.ceil() as i128),
            (Self::AfterRound, Number::Float(f)) => Number::Int(f.round() as i128),
            _ => number.clone(),
        }
    }
}

impl FromStr for CalcMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "前切り捨て" | "before_floor" => Ok(CalcMode::BeforeFloor),
            "後切り捨て" | "after_floor" => Ok(CalcMode::AfterFloor),
            "前切り上げ" | "before_ceil" => Ok(CalcMode::BeforeCeil),
            "後切り上げ" | "after_ceil" => Ok(CalcMode::AfterCeil),
            "前丸め" | "before_round" => Ok(CalcMode::BeforeRound),
            "後丸め" | "after_round" => Ok(CalcMode::AfterRound),
            "" => Ok(CalcMode::Float),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod expression {
        use super::*;

        mod eval {
            use super::*;

            #[test]
            fn success_when_valid_expr() {
                let case_01 = Expression::Number(Number::Float(-1.2));
                let mut errors = vec![];
                let result = case_01.eval(&CalcMode::BeforeFloor, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(-2));
            }

            #[test]
            fn failed_when_invalid_expr() {
                let case_01 = Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Float(1.2))),
                    Box::new(Expression::Number(Number::Float(3.4))),
                    Operator::Rem,
                ));
                let mut errors = vec![];
                assert!(case_01.eval(&CalcMode::Float, &mut errors).is_err());
                assert_eq!(
                    errors,
                    vec![EvalError::new(
                        Expression::Operation(Operation::new(
                            Box::new(Expression::Number(Number::Float(1.2))),
                            Box::new(Expression::Number(Number::Float(3.4))),
                            Operator::Rem,
                        )),
                        EvalErrorKind::RemTargetIsNotInt
                    )]
                );

                let case_02 = Expression::Operation(Operation::new(
                    Box::new(Expression::Number(Number::Int(i128::MAX))),
                    Box::new(Expression::Number(Number::Int(i128::MAX))),
                    Operator::Add,
                ));
                let mut errors = vec![];
                assert!(case_02.eval(&CalcMode::Float, &mut errors).is_err());
                assert_eq!(
                    errors,
                    vec![EvalError::new(case_02, EvalErrorKind::OverflowEvalResult)]
                );
            }
        }
    }

    mod operation {
        use super::*;

        mod eval {
            use super::*;

            #[test]
            fn success_when_valid_expression() {
                let case_01 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.5))),
                    Box::new(Expression::Number(Number::Float(2.3))),
                    Operator::Add,
                );
                let mut errors = vec![];
                let result = case_01.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(3.8));

                let case_02 = Operation::new(
                    Box::new(Expression::Operation(Operation::new(
                        Box::new(Expression::Number(Number::Int(2))),
                        Box::new(Expression::Number(Number::Float(1.1))),
                        Operator::Mul,
                    ))),
                    Box::new(Expression::Number(Number::Float(2.3))),
                    Operator::Add,
                );
                let mut errors = vec![];
                let result = case_02.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(4.5));

                let case_03 = Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(2))),
                    Operator::Add,
                );
                let mut errors = vec![];
                let result = case_03.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(3));

                let case_04 = Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(2))),
                    Operator::Sub,
                );
                let mut errors = vec![];
                let result = case_04.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(-1));

                let case_05 = Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(2))),
                    Operator::Mul,
                );
                let mut errors = vec![];
                let result = case_05.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(2));

                let case_06 = Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(2))),
                    Operator::Div,
                );
                let mut errors = vec![];
                let result = case_06.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(0));

                let case_07 = Operation::new(
                    Box::new(Expression::Number(Number::Int(1))),
                    Box::new(Expression::Number(Number::Int(2))),
                    Operator::Rem,
                );
                let mut errors = vec![];
                let result = case_07.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Int(1));

                let case_08 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.5))),
                    Box::new(Expression::Number(Number::Float(0.5))),
                    Operator::Add,
                );
                let mut errors = vec![];
                let result = case_08.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(2.0));

                let case_09 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.5))),
                    Box::new(Expression::Number(Number::Float(0.5))),
                    Operator::Sub,
                );
                let mut errors = vec![];
                let result = case_09.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(1.0));

                let case_10 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.5))),
                    Box::new(Expression::Number(Number::Float(0.5))),
                    Operator::Mul,
                );
                let mut errors = vec![];
                let result = case_10.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(0.75));

                let case_11 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.5))),
                    Box::new(Expression::Number(Number::Float(0.5))),
                    Operator::Div,
                );
                let mut errors = vec![];
                let result = case_11.eval(&CalcMode::Float, &mut errors).unwrap();
                assert_eq!(errors, vec![]);
                assert_eq!(result, Number::Float(3.0));
            }

            #[test]
            fn failed_when_invalid_expression() {
                let case_01 = Operation::new(
                    Box::new(Expression::Number(Number::Int(i128::MAX))),
                    Box::new(Expression::Number(Number::Int(10))),
                    Operator::Add,
                );
                let mut errors = vec![];
                assert!(case_01.eval(&CalcMode::Float, &mut errors).is_err());
                assert_eq!(
                    errors,
                    vec![EvalError::new(
                        Expression::Operation(case_01.clone()),
                        EvalErrorKind::OverflowEvalResult
                    )]
                );

                let case_02 = Operation::new(
                    Box::new(Expression::Number(Number::Float(1.2))),
                    Box::new(Expression::Number(Number::Int(10))),
                    Operator::Rem,
                );
                let mut errors = vec![];
                assert!(case_02.eval(&CalcMode::Float, &mut errors).is_err());
                assert_eq!(
                    errors,
                    vec![EvalError::new(
                        Expression::Operation(case_02.clone()),
                        EvalErrorKind::RemTargetIsNotInt,
                    )]
                );
            }
        }
    }

    mod calc_mode {
        use super::*;

        mod to_int_if_needed_on_before {
            use super::*;

            #[test]
            fn checking_value_on_float() {
                let case_1 = Number::Float(12.34);
                let case_2 = Number::Float(56.78);

                assert_eq!(
                    CalcMode::BeforeFloor.to_int_if_needed_on_before(&case_1),
                    Number::Int(12)
                );
                assert_eq!(
                    CalcMode::BeforeFloor.to_int_if_needed_on_before(&case_2),
                    Number::Int(56)
                );

                assert_eq!(
                    CalcMode::BeforeCeil.to_int_if_needed_on_before(&case_1),
                    Number::Int(13)
                );
                assert_eq!(
                    CalcMode::BeforeCeil.to_int_if_needed_on_before(&case_2),
                    Number::Int(57)
                );

                assert_eq!(
                    CalcMode::BeforeRound.to_int_if_needed_on_before(&case_1),
                    Number::Int(12)
                );
                assert_eq!(
                    CalcMode::BeforeRound.to_int_if_needed_on_before(&case_2),
                    Number::Int(57)
                );

                assert_eq!(
                    CalcMode::AfterFloor.to_int_if_needed_on_before(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::AfterFloor.to_int_if_needed_on_before(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::AfterCeil.to_int_if_needed_on_before(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::AfterCeil.to_int_if_needed_on_before(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::AfterRound.to_int_if_needed_on_before(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::AfterRound.to_int_if_needed_on_before(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::Float.to_int_if_needed_on_before(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::Float.to_int_if_needed_on_before(&case_2),
                    Number::Float(56.78)
                );
            }
        }

        mod to_int_if_needed_on_after {
            use super::*;

            #[test]
            fn checking_value_on_float() {
                let case_1 = Number::Float(12.34);
                let case_2 = Number::Float(56.78);

                assert_eq!(
                    CalcMode::AfterFloor.to_int_if_needed_on_after(&case_1),
                    Number::Int(12)
                );
                assert_eq!(
                    CalcMode::AfterFloor.to_int_if_needed_on_after(&case_2),
                    Number::Int(56)
                );

                assert_eq!(
                    CalcMode::AfterCeil.to_int_if_needed_on_after(&case_1),
                    Number::Int(13)
                );
                assert_eq!(
                    CalcMode::AfterCeil.to_int_if_needed_on_after(&case_2),
                    Number::Int(57)
                );

                assert_eq!(
                    CalcMode::AfterRound.to_int_if_needed_on_after(&case_1),
                    Number::Int(12)
                );
                assert_eq!(
                    CalcMode::AfterRound.to_int_if_needed_on_after(&case_2),
                    Number::Int(57)
                );

                assert_eq!(
                    CalcMode::BeforeFloor.to_int_if_needed_on_after(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::BeforeFloor.to_int_if_needed_on_after(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::BeforeCeil.to_int_if_needed_on_after(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::BeforeCeil.to_int_if_needed_on_after(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::BeforeRound.to_int_if_needed_on_after(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::BeforeRound.to_int_if_needed_on_after(&case_2),
                    Number::Float(56.78)
                );

                assert_eq!(
                    CalcMode::Float.to_int_if_needed_on_after(&case_1),
                    Number::Float(12.34)
                );
                assert_eq!(
                    CalcMode::Float.to_int_if_needed_on_after(&case_2),
                    Number::Float(56.78)
                );
            }
        }
    }
}
