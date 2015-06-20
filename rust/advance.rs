fn main() {
    let r = range(0, 5).advance(|x| {println!("{} ", x); x != 2});
    println!("{}", r);
}
