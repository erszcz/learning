use std::libc::{c_char, c_int, c_void, size_t, malloc, free};
use std::ptr;

static regex_t_size: uint = 64;

enum RegexCflags {
    REG_EXTENDED = 1,
}

extern {
    fn regcomp(preg: *mut u8, regex: *c_char, cflags: c_int) -> c_int;
    fn regexec(preg: *u8, string: *c_char,
               nmatch: size_t, pmatch: *mut u8, eflags: c_int) -> c_int;
}

pub struct Regex {
    priv preg: *u8
}

impl Regex {

    pub fn new(regex: &str) -> Regex {
        #[fixed_stack_segment];
        #[inline(never)];
        unsafe {
            let preg = malloc(regex_t_size as u64) as *mut u8;
            assert!(! ptr::is_null(preg));
            do regex.with_c_str() |c_regex| {
                regcomp(preg, c_regex, REG_EXTENDED as c_int);
            }
            Regex{preg: preg as *u8}
        }
    }

    pub fn find(&self, string: &str) -> bool {
        #[fixed_stack_segment];
        #[inline(never)];
        unsafe {
            do string.with_c_str() |c_string| {
                0 == regexec(self.preg, c_string,
                             0, ptr::mut_null(), 0 as c_int)
            }
        }
    }

}

impl Drop for Regex {
    fn drop(&mut self) {
        #[fixed_stack_segment];
        #[inline(never)];
        unsafe {
            free(self.preg as *c_void);
        }
    }
}

fn main() {
    let reg = Regex::new(std::os::args()[1]);
    let stdin = std::io::stdin();
    while ! stdin.eof() {
        let line = stdin.read_line();
        if reg.find(line) {
            println(line);
        }
    }
}
