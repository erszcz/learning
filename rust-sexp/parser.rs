#![feature(phase, globs)]
#[phase(syntax, link)] extern crate log;

// S Expression Parser
//
// This parser should correspond to the following grammar:
//
// sexp :: ATOM
//       | list
// list :: LPAREN members RPAREN
//       | LPAREN RPAREN
// members :: sexp
//          | sexp members
//
// Terminals: ATOM LPAREN RPAREN

use lexer::*;
mod lexer;

#[deriving(Eq, Show, Clone)]
enum SExp {
  Atom (~str),
  List (~[SExp])
}

#[deriving(Eq, Show)]
enum ParseResult<'a> {
  Ok (SExp),
  Error (&'a str)
}

fn parse_sexp(tokens: &[Token]) -> ParseResult {
  log!(log::INFO, "parse_sexp: {}", tokens);
  match tokens {
    [] =>
      Error ("no tokens"),
    [ATOM (ref atom)] =>
      Ok (Atom (atom.to_owned())),
    _ =>
      parse_list(tokens)
  }
}

fn parse_list(tokens: &[lexer::Token]) -> ParseResult {
  log!(log::INFO, "parse_list: {}", tokens);
  let last = tokens.len()-1;
  match tokens {
    [LPAREN, RPAREN] =>
      Ok (List (~[])),
    _ if tokens[0] == LPAREN && tokens[last] == RPAREN =>
      parse_members(tokens.slice(1, last)),
    _ =>
      Error ("parse_list: tokens do not form a list")
  }
}

fn parse_members(tokens: &[lexer::Token]) -> ParseResult {
  log!(log::INFO, "parse_members: {}", tokens);
  let mut midpoint = tokens.len();
  'g: loop {
    let tokens_before_midpoint = tokens.slice(0, midpoint);
    match parse_sexp(tokens_before_midpoint) {
      Error (_) => {
        log!(log::INFO, "parse_members: parse_sexp error: {}",
             tokens_before_midpoint);
        if midpoint >= 2 {
          midpoint -= 1;
          continue 'g
        } else {
          return Error ("can't match initial sexp")
        }
      },
      Ok (ref sexp) if midpoint == tokens.len() => {
        log!(log::INFO, "parse_members: matched 1st rule");
        return Ok (List (~[sexp.clone()]))
      },
      Ok (ref sexp) => {
        log!(log::INFO, "parse_members: matched 2nd rule");
        let tokens_past_midpoint = tokens.slice(midpoint, tokens.len());
        match parse_members(tokens_past_midpoint) {
          Ok (List (inner_members)) => {
            assert_eq!(1, inner_members.len());
            let inner_sexp = inner_members[0];
            return Ok (List (~[sexp.clone(), inner_sexp]))
          },
          Ok (Atom (_)) => {
            log!(log::INFO, "parse_members: inner member can't be an atom");
            return Error ("parse_members: inner member can't be an atom")
          }
          Error (reason) => {
            log!(log::INFO, "parse_members: inner members error");
            return Error (reason)
          }
        }
      }
    }
  }
}

fn unwrap(parse_result: ParseResult) -> SExp {
  match parse_result {
    Ok (sexp) => sexp,
    Error (reason) => fail!(reason.to_owned())
  }
}

fn tokeparse(s: &str) -> SExp {
  unwrap( parse_sexp( lexer::tokenize(s) ) )
}

#[test]
fn test_parse_1() {
  assert_eq!(Atom (~"asd"), tokeparse("asd"));
}

#[test]
fn test_parse_2() {
  assert_eq!(List (~[Atom (~"asd"), Atom (~"qwe")]),
             tokeparse("(asd qwe)"));
}

#[test]
fn test_parse_3() {
  assert_eq!(List (~[List (~[Atom (~"asd")]), Atom (~"qwe")]),
             tokeparse("((asd) qwe)"));
}
