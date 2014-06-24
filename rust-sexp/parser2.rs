#![feature(phase, globs)]
#[phase(syntax, link)] extern crate log;

fn tokenize(s: &str) -> ~[&str] {
    let t = std::str::replace(s, "(", " ( ");
    let u = std::str::replace(t, ")", " ) ");
    u.words().collect()
}

#[deriving(Eq, Show, Clone)]
pub enum SExp {
    Atom (~str),
    List (~[SExp])
}

fn parse(tokens: ~[&str]) -> SExp {
    log!(log::INFO, "parse({})", tokens);
    println!("tokens = {}", tokens);
    let (_, sexp) = do_parse(0, tokens);
    sexp
}

fn do_parse(mut i: uint, tokens: &[&str]) -> (uint, SExp) {
    log!(log::INFO, "do_parse({}, {})", i, tokens);
    if tokens.is_empty() { fail!("unexpected EOF") }
    let token = tokens[i];
    if token == ")" { fail!("unexpected )") }
    if token != "(" { return ( 1, Atom (token.to_owned()) ) }
    let mut sexps: ~[SExp] = ~[];
    i += 1;
    while tokens[i] != ")" {
        log!(log::INFO, "tokens[{}] = '{}'", i, tokens[i]);
        let (n, sexp) = do_parse(i, tokens);
        sexps.push(sexp);
        i += n;
    }
    (i, List (sexps))
}

#[test]
fn tokenize_test() {
    assert_eq!(~["(", "set!", "a",
                      "(", "*", "(", "+", "1", "2", ")", "3", ")", ")"],
               tokenize("(set! a (* (+ 1 2) 3))"));
    assert_eq!(~["(", ")", "asd"], tokenize("()asd"));
    assert_eq!(~["asd"], tokenize("asd"));
    assert_eq!(~["asd", "qwe"], tokenize("asd qwe"));
    assert_eq!(~["asd", ")", "(", "qwe"], tokenize("asd)(qwe"));
}

fn tokeparse(s: &str) -> SExp {
  parse(tokenize(s))
}

#[test]
fn test_parse_1() {
  assert_eq!(Atom (~"asd"), tokeparse("asd"));
}

#[test]
fn test_parse_2() {
  let tokens = tokenize(~"(asd qwe)");
  println!("test_parse_2: tokens = {}", tokens);
  assert_eq!(List (~[Atom (~"asd"), Atom (~"qwe")]),
             parse(tokens));
}

#[test]
fn test_parse_3() {
  assert_eq!(List (~[List (~[Atom (~"asd")]), Atom (~"qwe")]),
             tokeparse("((asd) qwe)"));
}

#[test]
fn test_parse_4() {
  assert_eq!(List (~[List (~[Atom (~"asd")]), List (~[Atom (~"qwe")])]),
             tokeparse("((asd) (qwe))"));
}
