use std::fs::OpenOptions;
use std::io::{ Error, Read, Seek, SeekFrom, Write };

fn main() {
    let mut opts = OpenOptions::new();
    opts.read(true);
    opts.write(true);
    opts.append(true);
    match opts.open("example.dat") {
        Err (_) => panic!("OpenOptions::open"),
        Ok (mut f) => {
            let mut buf: Vec<u8> = vec![];
            f.read_to_end(&mut buf);
            println!("{:?}", buf);
            f.seek(SeekFrom::End(0));
            f.write_fmt(format_args!("\nanother line"));
        }
    }
}
