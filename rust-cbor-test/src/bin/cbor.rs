extern crate serde_cbor;
extern crate serde_json;

use serde_cbor::{ from_reader, Value };
use serde_cbor::ser::{ to_writer };
use serde_json::{ to_string_pretty };

fn main() {
    let value: Value = from_reader(std::io::stdin()).unwrap();
    let args = std::env::args();
    match args.last().expect("cbor: --to-json or --to-cbor?").as_str() {
        "--to-json" => {
            println!("{}", to_string_pretty(&value).unwrap());
        }
        "--to-cbor" => {
            let _ = to_writer(&mut std::io::stdout(), &value).unwrap();
        }
        _ => panic!("cbor: unknown option")
    }
}
