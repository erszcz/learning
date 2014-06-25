#![feature(phase, globs)]
#[phase(plugin, link)] extern crate log;

fn tokenize(r: &str) -> Vec<String> {
    r.to_string().replace("(", " ( ").replace(")", " ) ")
     .as_slice().words().map(|s| s.to_string()).collect()
}

#[deriving(PartialEq, Eq, Show, Clone)]
pub enum SExp {
    Atom (Box<String>),
    List (Vec<SExp>)
}

fn parse(tokens: Vec<String>) -> SExp {
    let (_, sexp) = do_parse(0, &tokens);
    sexp
}

fn do_parse(mut i: uint, tokens: &Vec<String>) -> (uint, SExp) {
    if tokens.is_empty() { fail!("unexpected EOF") }
    let token = tokens.get(i);
    if *token == ")".to_string() { fail!("unexpected )") }
    if *token != "(".to_string() { return ( 1, Atom (box token.to_string()) ) }
    let mut sexps: Vec<SExp> = Vec::new();
    i += 1;
    let mut ntotal = 2;
    while i < tokens.len() && *tokens.get(i) != ")".to_string() {
        let (n, sexp) = do_parse(i, tokens);
        sexps.push(sexp);
        ntotal += n;
        i += n;
    }
    (ntotal, List (sexps))
}

// needed for tests
fn tokens(a: &[&str]) -> Vec<String> {
    a.iter().map(|s| s.to_string()).collect()
}

fn atom(s: &str) -> SExp {
    Atom (box s.to_string())
}

fn list(sexps: &[SExp]) -> SExp {
    List ( sexps.iter().map(|sexp| sexp.clone()).collect() )
}

#[test]
fn tokenize_test() {
    assert_eq!(tokens(["(", "set!", "a",
                            "(", "*", "(", "+", "1", "2", ")", "3", ")", ")"]),
               tokenize("(set! a (* (+ 1 2) 3))"));
    assert_eq!(tokens(["(", ")", "asd"]), tokenize("()asd"));
    assert_eq!(tokens(["asd"]), tokenize("asd"));
    assert_eq!(tokens(["asd", "qwe"]), tokenize("asd qwe"));
    assert_eq!(tokens(["asd", ")", "(", "qwe"]), tokenize("asd)(qwe"));
  }

fn tokeparse(s: &str) -> SExp {
  parse(tokenize(s))
}

#[test]
fn test_parse_1() {
  assert_eq!(atom("asd"), tokeparse("asd"));
}

#[test]
fn test_parse_2() {
  let tokens = tokenize("(asd qwe)");
  println!("test_parse_2: tokens = {}", tokens);
  assert_eq!(list ([atom ("asd"), atom ("qwe")]),
             parse(tokens));
}

#[test]
fn test_parse_3() {
  assert_eq!(list ([list ([atom ("asd")]), atom ("qwe")]),
             tokeparse("((asd) qwe)"));
}

#[test]
fn test_parse_4() {
  assert_eq!(list ([list ([atom ("asd")]),
                    list ([atom ("qwe")])]),
             tokeparse("((asd) (qwe))"));
}

#[test]
fn test_parse_5() {
  assert_eq!(list ([atom ("asd"),
                    atom ("qwe"),
                    atom ("zxc"),
                    atom ("fgh")]),
             tokeparse("(asd qwe zxc fgh)"));
}

#[test]
fn test_parse_6() {
  assert_eq!(list ([atom ("asd"),
                    atom ("qwe"),
                    atom ("zxc"),
                    list ([atom ("fgh")])]),
             tokeparse("(asd qwe zxc (fgh))"));
}

#[test]
fn test_parse_7() {
  assert_eq!(list ([list ([atom ("zxc")]),
                    list ([atom ("fgh")])]),
             tokeparse("((zxc) (fgh))"));
}

#[test]
fn test_parse_8() {
  assert_eq!(list ([atom ("zxc"),
                    list ([atom ("fgh")])]),
             tokeparse("(zxc (fgh))"));
}

#[test]
fn test_parse_9() {
  assert_eq!(list ([atom ("qwe"),
                    atom ("zxc"),
                    list ([atom ("fgh")])]),
             tokeparse("(qwe zxc (fgh))"));
}

#[test]
fn test_parse_10() {
  assert_eq!(list ([atom ("qwe"),
                    list ([atom ("zxc")]),
                    list ([atom ("fgh")])]),
             tokeparse("(qwe (zxc) (fgh))"));
}

#[test]
fn test_parse_11() {
  assert_eq!(list ([atom ("qwe"),
                    list ([atom ("zxc"), atom ("bnm")]),
                    list ([atom ("fgh")])]),
             tokeparse("(qwe (zxc bnm) (fgh))"));
}

#[test]
fn test_parse_12() {
  assert_eq!(list ([atom ("+"),
                    list ([atom ("*"), atom ("2"), atom ("3")]),
                    list ([atom ("/"), atom ("12"), atom ("4")]),
                    list ([atom ("magic-constant-lookup")])]),
             tokeparse("(+ (* 2 3) (/ 12 4) (magic-constant-lookup))"));
}
