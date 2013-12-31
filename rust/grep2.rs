fn grep(needle: &str) {
    let stdin = std::io::stdin();
    while ! stdin.eof() {
        let line = stdin.read_line();
        if line.contains(needle) {
            println(line);
        }
    }
}

fn main() {
    let args = std::os::args().clone();
    grep(args[1]);
}
