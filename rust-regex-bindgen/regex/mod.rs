use std::cast;
use std::libc::c_void;
use std::mem;

mod c;

pub struct Regex {
    priv regex: Option<c::regex_t>
}

impl Regex {

    pub fn new(pattern: &str) -> Regex {
        unsafe {
            let mut c_regex : c::regex_t = mem::init::<c::regex_t>();
            pattern.with_c_str(|c_pattern| {
                match c::regcomp(&mut c_regex, c_pattern, 0) {
                    0 => return Regex{regex: Some(c_regex)},
                    _ => return Regex{regex: None}
                }
            })
        }
    }

    pub fn find(&self, string: &str) -> bool {
        unsafe {
            let null : c_void = mem::init::<c_void>();
            string.with_c_str(|c_string| {
                match self.regex {
                    Some (c_regex) =>
                        0 == c::regexec(&c_regex, c_string, 0, null, 0),
                    None =>
                        false
                }
            })
        }
    }

}

impl Drop for Regex {
    fn drop(&mut self) {
        unsafe {
            match self.regex {
                Some (mut c_regex) => c::regfree(&mut c_regex),
                None               => ()
            }
        }
    }
}
