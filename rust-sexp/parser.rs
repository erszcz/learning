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
    let tokens_past_midpoint = tokens.slice(midpoint, tokens.len());
    let sexp_result = parse_sexp(tokens_before_midpoint);
    match parse_members_match(midpoint,
                              tokens.len(),
                              tokens_past_midpoint,
                              sexp_result) {
      Matched1st (result) => {
        log!(log::INFO, "parse_members: matched 1st rule");
        return result
      },
      Matched2nd (result) => {
        log!(log::INFO, "parse_members: matched 2nd rule");
        return result
      },
      Retry (new_midpoint) => {
        midpoint = new_midpoint;
        continue 'g
      },
      MatchError (reason) =>
        return Error (reason)
    }
  }
}

enum ParseMembersResult<'a> {
  Matched1st (ParseResult<'a>),
  Matched2nd (ParseResult<'a>),
  Retry (uint),
  MatchError (&'a str)
}

fn parse_members_match(midpoint: uint,
                       tokens_len: uint,
                       tokens_past_midpoint: &[lexer::Token],
                       sexp_result: ParseResult) -> ParseMembersResult {
  match sexp_result {
    Error (_) => {
      if midpoint >= 2 {
        Retry (midpoint - 1)
      } else {
        MatchError ("can't match initial sexp")
      }
    },
    Ok (ref sexp) if midpoint == tokens_len => {
      let parse_result = Ok (List (~[sexp.clone()]));
      Matched1st (parse_result)
    },
    Ok (ref sexp) => {
      match parse_members(tokens_past_midpoint) {
        Ok (List (inner_members)) => {
          assert_eq!(1, inner_members.len());
          let inner_sexp = inner_members[0];
          let members_2nd = Ok (List (~[sexp.clone(), inner_sexp]));
          Matched2nd (members_2nd)
        },
        Ok (Atom (_)) => {
          log!(log::INFO, "parse_members: inner member can't be an atom");
          MatchError ("parse_members: inner member can't be an atom")
        }
        Error (_) => {
          log!(log::INFO, "parse_members: inner members error");
          MatchError ("parse_members: inner members error")
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
