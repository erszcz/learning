#![feature(phase, globs)]
#[phase(plugin, link)] extern crate log;

use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Show;

fn atom(s: &str) -> SExp {
    Atom (box s.to_string())
}

fn list(sexps: &[SExp]) -> SExp {
    List ( sexps.iter().map(|sexp| sexp.clone()).collect() )
}

#[deriving(PartialEq, Eq, Clone)]
pub enum SExp {
    Atom (Box<String>),
    List (Vec<SExp>)
}

impl Show for SExp {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Atom (ref a) => write!(f, "{}", a),
            List (ref sexps) => {
                try!(write!(f, "("));
                for (i, sexp) in sexps.iter().enumerate() {
                    if i != 0 { try!(write!(f, " ")); }
                    try!(write!(f, "{}", sexp));
                }
                write!(f, ")")
            }
        }
    }
}

#[test]
fn test_show_sexp() {
    assert_eq!("asd".to_string(), format!("{}", atom ("asd")));
    assert_eq!("()".to_string(), format!("{}", list ([])));
    assert_eq!("(asd)".to_string(), format!("{}", list ([atom ("asd")])));
    assert_eq!("(+ (/ 12 4) (val-of-pi))".to_string(),
               format!("{}", list ([atom ("+"),
                                    list ([atom ("/"), atom ("12"), atom ("4")]),
                                    list ([atom ("val-of-pi")])])));
}
