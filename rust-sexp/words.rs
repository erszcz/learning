fn tokens(s: &str) -> ~[&str] {
    let t = std::str::replace(s, "(", " ( ");
    let u = std::str::replace(t, ")", " ) ");
    u.words().collect()
}

fn main() {
    let s = ~"(a string)";
    let t = std::str::replace(s, "(", " ( ");
    let u = std::str::replace(t, ")", " ) ");
    for w in u.words() {
        print!("{} ", w);
    }
    println!("");
}
