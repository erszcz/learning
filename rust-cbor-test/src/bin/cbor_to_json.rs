extern crate serde_cbor;
extern crate serde_json;

use serde_cbor::{ from_reader, Value };
use serde_json::{ to_string_pretty };

fn main() {
    let value: Value = from_reader(std::io::stdin()).unwrap();
    println!("{}", to_string_pretty(&value).unwrap());
}
