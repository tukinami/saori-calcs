const REPLACE_LIST: [(char, char); 20] = [
    ('０', '0'),
    ('１', '1'),
    ('２', '2'),
    ('３', '3'),
    ('４', '4'),
    ('５', '5'),
    ('６', '6'),
    ('７', '7'),
    ('８', '8'),
    ('９', '9'),
    ('＋', '+'),
    ('−', '-'),
    ('＊', '*'),
    ('×', '*'),
    ('／', '/'),
    ('÷', '/'),
    ('％', '%'),
    ('（', '('),
    ('）', ')'),
    ('．', '.'),
];

const REPLACED_CHAR: char = 'a';

pub(crate) fn preprocess(source: &str) -> String {
    source
        .chars()
        .map(|v| {
            REPLACE_LIST
                .iter()
                .find(|(target, _)| &v == target)
                .map(|(_, result)| *result)
                .unwrap_or_else(|| if v.is_ascii() { v } else { REPLACED_CHAR })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    mod preprocess {
        use super::*;

        #[test]
        fn checking_value() {
            let case_01 = "０１２３４５６７８９＋−＊×／÷％（）．";
            assert_eq!(preprocess(case_01), "0123456789+-**//%().".to_string());

            let case_02 = "0123456789+-**//%().";
            assert_eq!(preprocess(case_02), "0123456789+-**//%().".to_string());

            let case_03 = "あああ";
            assert_eq!(preprocess(case_03), "aaa".to_string());

            let case_04 = "abc";
            assert_eq!(preprocess(case_04), "abc".to_string());
        }
    }
}
