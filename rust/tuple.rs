type Pair<'a> = (int, &'a str);

fn main() {
    let p: Pair<'static> = (10, "hello");
    let (_,b) = p;
    assert!(b != "world");
}
