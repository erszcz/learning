use std::fs::File;
use std::io::{ Error, Write };

fn main() {
    match File::create("example.dat") {
        Err (_) => panic!("create"),
        Ok (mut f) => {
            f.write_fmt(format_args!("{:?}", vec![1, 2, 3, 4]));
        }
    }
}
