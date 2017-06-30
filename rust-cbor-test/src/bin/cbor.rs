extern crate serde_cbor;
extern crate serde_json;

use serde_cbor::{ from_reader, Value };
use serde_cbor::ser::{ to_writer };
use serde_json::{ to_string_pretty };

fn main() {
    let args = std::env::args();
    match args.last().expect("cbor: --to-json or --to-cbor?").as_str() {
        "--to-json" => {
            let value: Value = from_reader(std::io::stdin()).unwrap();
            println!("{}", to_string_pretty(&value).unwrap());
        }
        "--to-cbor" => {
            let value: Value = from_reader(std::io::stdin()).unwrap();
            let _ = to_writer(&mut std::io::stdout(), &value).unwrap();
        }
        _ => panic!("cbor: unknown option")
    }
}
