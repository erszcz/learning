use std::fs::File;
use std::io::{ Error, Read };

fn main() {
    match File::open("example.dat") {
        Err (_) => panic!("open"),
        Ok (mut f) => {
            let mut buf: Vec<u8> = vec![];
            f.read_to_end(&mut buf);
            println!("{:?}", buf);
        }
    }
}
