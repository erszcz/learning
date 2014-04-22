#![feature(globs)]

mod regex;

fn main() {
    let regex = regex::Regex::new(std::os::args()[1]);
    let mut stdin = std::io::stdin();
    for maybe_line in stdin.lines() {
        match maybe_line {
            Ok (line) => if regex.find(line) {
                print!("{}", line);
            },
            Err (_) => {}
        }
    }
}
