// fails on:
// - negative_mixed
// - negative_mixed_nested
// - negative_mixed_more
//fn is_well_formed(text: &str) -> bool {
//    let mut nesting = 0;
//    let mut i = 0;
//    while i < text.len() {
//        match text.char_at(i) {
//            '(' => nesting += 1,
//            ')' => nesting -= 1,
//            ___ => ()
//        }
//        i += 1;
//    }
//    nesting == 0
//}

fn is_well_formed(text: &str) -> bool {
    parse_open(0, text)
}

fn parse_open(nesting: uint, text: &str) -> bool {
    if nesting == 0 && text == "" { return true; }
    if text == ""                 { return false; }
    if text.char_at(0) != '('     { return false; }
    let new_nesting = nesting + 1;
    let new_slice = text.slice_chars(1, text.len());
    parse_open(new_nesting, new_slice) || parse_close(new_nesting, new_slice)
}

fn parse_close(nesting: uint, text: &str) -> bool {
    //if nesting == 0 && text == "" { return true; }
    if text == ""                 { return false; }
    if text.char_at(0) != ')'     { return false; }
    let new_nesting = nesting - 1;
    let new_slice = text.slice_chars(1, text.len());
    parse_open(new_nesting, new_slice) || parse_close(new_nesting, new_slice)
}

#[test]
fn empty() { assert!(is_well_formed("")); }

#[test]
fn one() { assert!(is_well_formed("()")); }

#[test]
fn negative_open() { assert!(! is_well_formed("(")); }

#[test]
fn negative_close() { assert!(! is_well_formed(")")); }

#[test]
fn nested() { assert!(is_well_formed("(())")); }

#[test]
fn consecutive() { assert!(is_well_formed("()()")); }

#[test]
fn negative_non_matching() { assert!(! is_well_formed("(()")); }

#[test]
fn negative_no_close() { assert!(! is_well_formed("()(")); }

#[test]
fn negative_mixed() { assert!(! is_well_formed(")(")); }

#[test]
fn negative_mixed_nested() { assert!(! is_well_formed("))((")); }

#[test]
fn negative_mixed_more() { assert!(! is_well_formed(")())((")); }
