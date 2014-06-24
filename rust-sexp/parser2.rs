#![feature(phase, globs)]
#[phase(plugin, link)] extern crate log;

fn tokenize(s: &str) -> Vec<&str> {
    let t = std::str::replace(s.as_slice(), "(", " ( ");
    let u = std::str::replace(t.as_slice(), ")", " ) ");
    u.as_slice().words().collect()
}

#[deriving(PartialEq, Eq, Show, Clone)]
pub enum SExp {
    Atom (Box<String>),
    List (Vec<SExp>)
}

fn parse(tokens: Vec<&str>) -> SExp {
    log!(log::INFO, "parse({})", tokens);
    println!("tokens = {}", tokens);
    let (_, sexp) = do_parse(0, tokens);
    sexp
}

fn do_parse(mut i: uint, tokens: Vec<&str>) -> (uint, SExp) {
    log!(log::INFO, "do_parse({}, {})", i, tokens);
    if tokens.is_empty() { fail!("unexpected EOF") }
    let token = *tokens.get(i);
    if token == ")" { fail!("unexpected )") }
    if token != "(" { return ( 1, Atom (box token.to_owned()) ) }
    let mut sexps: Vec<SExp> = Vec::new();
    i += 1;
    while *tokens.get(i) != ")" {
        log!(log::INFO, "tokens[{}] = '{}'", i, tokens.get(i));
        let (n, sexp) = do_parse(i, tokens);
        sexps.push(sexp);
        i += n;
    }
    (i, List (sexps))
}

#[test]
fn tokenize_test() {
    assert_eq!(vec!["(", "set!", "a",
                         "(", "*", "(", "+", "1", "2", ")", "3", ")", ")"],
               tokenize("(set! a (* (+ 1 2) 3))"));
    assert_eq!(vec!["(", ")", "asd"], tokenize("()asd"));
    assert_eq!(vec!["asd"], tokenize("asd"));
    assert_eq!(vec!["asd", "qwe"], tokenize("asd qwe"));
    assert_eq!(vec!["asd", ")", "(", "qwe"], tokenize("asd)(qwe"));
}

fn tokeparse(s: &str) -> SExp {
  parse(tokenize(s))
}

#[test]
fn test_parse_1() {
  assert_eq!(Atom (box "asd".to_string()), tokeparse("asd"));
}

#[test]
fn test_parse_2() {
  let tokens = tokenize("(asd qwe)");
  println!("test_parse_2: tokens = {}", tokens);
  assert_eq!(List (vec![Atom (box "asd".to_string()), Atom (box "qwe".to_string())]),
             parse(tokens));
}

#[test]
fn test_parse_3() {
  assert_eq!(List (vec![List (vec![Atom (box "asd".to_string())]), Atom (box "qwe".to_string())]),
             tokeparse("((asd) qwe)"));
}

#[test]
fn test_parse_4() {
  assert_eq!(List (vec![List (vec![Atom (box "asd".to_string())]),
                        List (vec![Atom (box "qwe".to_string())])]),
             tokeparse("((asd) (qwe))"));
}
