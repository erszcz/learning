fn is_substr(haystack: &str, needle: &str) -> bool {
    let mut i: uint = 0;
    while i < haystack.len() {
        let mut j: uint = 0;
        while   i+j < haystack.len()
                && j < needle.len()
                && haystack[i+j] == needle[j] {
            j += 1;
        }
        if j == needle.len() {
            return true;
        }
        i += 1;
    }
    false
}

fn test_is_substr() {
    print!("Testing is_substr... ");
    let example = "ala ma kota";
    assert!(true == is_substr(" ", ""));
    assert!(true == is_substr(example, "kota"));
    assert!(true == is_substr(example, "ala"));
    assert!(true == is_substr(example, "a m"));
    assert!(false == is_substr(example, "konia"));
    assert!(false == is_substr(example, "al "));
    println!("ok.");
}

fn grep(needle: &str) {
    for l in std::io::stdin().lines() {
        match l {
            Ok(line) =>
                if is_substr(line, needle) {
                    print!("{}", line);
                },
            Err(_) => {}
        }
    }
}

fn main() {
    let args = std::os::args().clone();
    if ~"-t" == args[1] || ~"--test" == args[1] {
        test_is_substr()
    } else {
        grep(args[1])
    }
}
