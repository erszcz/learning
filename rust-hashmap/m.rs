use std::collections::HashMap;

fn main() {
    let mut m: HashMap<String, String> = HashMap::new();
    m.insert("a".to_string(), "1".to_string());
    println!("{:?}", m);
    println!("contains_key \"a\": {:?}", m.contains_key("a"));
}
