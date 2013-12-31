use std::libc::{c_char, c_int, size_t};
use std::ptr;
use std::vec;

static regex_t_size: uint = 64;

enum RegexCflags {
    REG_EXTENDED = 1,
}

extern {
    fn regcomp(preg: *mut u8, regex: *c_char, cflags: c_int) -> c_int;
    fn regexec(preg: *u8, string: *c_char,
               nmatch: size_t, pmatch: *mut u8, eflags: c_int) -> c_int;
}

#[fixed_stack_segment]
fn main() {
    let mut reg: ~[u8] = std::vec::with_capacity(regex_t_size);
    unsafe {
        do "asd".with_c_str() |c_buffer| {
            regcomp(vec::raw::to_mut_ptr(reg),
                    c_buffer, REG_EXTENDED as c_int);
        }
    }
    let mut found = false;
    unsafe {
        do "asdqwe".with_c_str() |c_buffer| {
            if regexec(vec::raw::to_ptr(reg),
                       c_buffer, 0, ptr::mut_null(), 0 as c_int) == 0 {
                found = true;
            }
        }
    }
    println!("found = {}", found);
}
