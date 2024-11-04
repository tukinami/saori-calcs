use crate::calc::calc;
use crate::request::*;
use crate::response::*;

/// load時に呼ばれる関数
pub fn load(_path: &str) {}

/// unload時に呼ばれる関数
pub fn unload(_path: &str) {}

/// request GET Version時に呼ばれる関数
pub fn get_version(_path: &str, _request: &SaoriRequest, response: &mut SaoriResponse) {
    response.set_result(String::from(env!("CARGO_PKG_VERSION")));
}

/// request EXECUTE時に呼ばれる関数
/// メインの処理はここに記述する
pub fn execute(_path: &str, request: &SaoriRequest, response: &mut SaoriResponse) {
    let args = request.argument();
    // let mut path = PathBuf::from(path);
    // if !path.is_dir() {
    //     path.pop();
    // }

    const ERROR_HEADER: &str = "Error_";

    let (calc_mode_str, expression_str) = match (args.first(), args.get(1)) {
        (Some(calc), Some(exp)) => (calc, exp),
        (_, None) | (None, _) => {
            response.set_result(format!("{}引数エラー: 引数が足りません", ERROR_HEADER));
            return;
        }
    };

    if expression_str.starts_with(ERROR_HEADER) {
        response.set_result(format!(
            "{}エラー未修正: 前回のエラーが修正されていません。",
            ERROR_HEADER
        ));
        return;
    }

    match calc(calc_mode_str, expression_str) {
        Ok(result) => response.set_result(result),
        Err(error) => response.set_result(format!("{}{}", ERROR_HEADER, error)),
    }
}
