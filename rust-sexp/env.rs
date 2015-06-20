#![feature(phase, globs)]
#[phase(plugin, link)] extern crate log;

use std::collections::HashMap;
use std::gc::Gc;
use sexp::SExp;

mod sexp;

#[deriving(PartialEq, Eq)]
struct Env {
    outer: Option<Gc<Env>>,
    items: HashMap<String, SExp>
}

impl Env {

    fn new() -> Env {
        Env::inner(None)
    }

    fn inner(outer: Option<Gc<Env>>) -> Env {
        Env{outer: outer,
            items: HashMap::new()}
    }

    fn find<'a>(&'a self, s: String) -> Option<&'a SExp> {
        let maybe_sexp: Option<&SExp> = self.items.find_equiv(&s);
        match maybe_sexp {
            Some (sexp) => Some (sexp),
            None => match self.outer {
                Some (ref env) => env.find(s),
                None => None
            }
        }
    }
}

#[test]
fn test_new_top_level() {
    let top_level = Env::new();
    assert!(top_level.outer.is_none());
    assert!(top_level.items.is_empty());
}

#[test]
fn test_new_inner() {
    let top_level = Env::new();
    let inner = Env::inner(top_level);
    assert!(inner.outer.is_some());
    assert_eq!(top_level, *inner.outer.get_ref());
    assert!(inner.items.is_empty());
}

//#[test]
//fn test_find() {
//    ;
//}
