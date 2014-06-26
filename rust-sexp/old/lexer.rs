//#![feature(phase)]
//#[phase(syntax, link)]
extern crate log;

#[deriving(Eq, Show)]
pub enum Token {
    LPAREN,
    RPAREN,
    ATOM (~str)
}

struct Lexer {
    i : uint,
    tokens : ~[Token],
    atom_start : uint,
    parsing_atom : bool,
}

impl Lexer {
    fn new() -> Lexer {
        Lexer {
            i: 0,
            tokens: ~[],
            atom_start: 0,
            parsing_atom: false
        }
    }
}

pub fn tokenize(s: &str) -> ~[Token] {
    let mut state = Lexer::new();
    while state.i < s.len() {
        match s.char_at(state.i) {
            '(' => {
                //log!(log::INFO, "{} LPAREN", state.i);
                stop_atom(&mut state, s);
                push(&mut state, LPAREN);
            },
            ')' => {
                //log!(log::INFO, "{} RPAREN", state.i);
                stop_atom(&mut state, s);
                push(&mut state, RPAREN);
            },
            ' ' => {
                //log!(log::INFO, "{} whitespace", state.i);
                stop_atom(&mut state, s);
            },
            ___ => {
                start_atom(&mut state);
            }
        }
        state.i += 1;
    }
    //log!(log::INFO, "{} end of input\n", state.i);
    stop_atom(&mut state, s);
    state.tokens
}

fn push(state: &mut Lexer, token: Token) {
    state.tokens.push(token);
}

fn start_atom(state: &mut Lexer) {
    //log!(log::INFO, "{} atom start", state.i);
    if !state.parsing_atom {
        state.parsing_atom = true;
        state.atom_start = state.i
    } else {
        //log!(log::INFO, "{} inside atom", state.i);
    }
}

fn stop_atom(state: &mut Lexer, s: &str) {
    if state.parsing_atom {
        push(state, ATOM (s.slice(state.atom_start, state.i).to_owned()));
        state.parsing_atom = false;
    }
}

#[test]
fn test_tokenize() {
    assert_eq!(~[], tokenize(""));
    assert_eq!(~[ATOM (~"asd")], tokenize("asd"));
    assert_eq!(~[ATOM (~"asd"), ATOM (~"qwe")], tokenize("asd qwe"));
    assert_eq!(~[LPAREN, LPAREN], tokenize("(("));
    assert_eq!(~[LPAREN, RPAREN], tokenize("()"));
    assert_eq!(~[LPAREN, LPAREN], tokenize(" ( ( "));
    assert_eq!(~[RPAREN, LPAREN], tokenize(" )  ( "));
    assert_eq!(~[RPAREN, LPAREN, RPAREN, LPAREN], tokenize(")()("));
    assert_eq!(~[RPAREN, LPAREN], tokenize(") ("));
    assert_eq!(~[RPAREN, LPAREN], tokenize(") ("));
    assert_eq!(~[LPAREN, ATOM (~"asd"), ATOM (~"qwe"), RPAREN],
               tokenize("(asd qwe)"));
}
