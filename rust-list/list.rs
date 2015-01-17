#![feature(box_syntax)]

use std::fmt::{self, Show};

enum List<T> {
    Nil,
    Cons (T, Box<List<T>>)
}

impl<T> List<T> {

    fn new() -> List<T> { List::Nil }

    fn insert(self, e: T) -> List<T> { List::Cons (e, box self) }

}

impl<T: fmt::String> fmt::String for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut this = self;
        loop {
            match this {
                &List::Nil => {
                    write!(f, "(nil)");
                    return Ok (())
                }
                &List::Cons (ref e, ref l) => {
                    write!(f, "{} -> ", e);
                    this = &**l;
                }
            }
        }
    }
}

fn main() {
    let mut l: List<i64> = List::new();
    l = l.insert(3);
    l = l.insert(2);
    l = l.insert(1);
    println!("{}", l);
}
