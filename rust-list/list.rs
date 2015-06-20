#[derive(Debug)]
enum List<T> {
    Nil,
    Cons (T, Box<List<T>>)
}

impl<T> List<T> {

    fn new() -> List<T> { List::Nil }

    fn insert(self, e: T) -> List<T> { List::Cons (e, Box::new(self)) }

}

fn main() {
    let mut l: List<i64> = List::new();
    l = l.insert(3);
    l = l.insert(2);
    l = l.insert(1);
    println!("{:?}", l);
}
