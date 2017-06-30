extern crate cbor;
extern crate rustc_serialize;

use cbor::{ Decoder };
use rustc_serialize::json::{ Json, ToJson };

fn main() {
    let mut d = Decoder::from_reader(std::io::stdin());
    let cbor = d.items().next();
    println!("{:?}", cbor);
}
