#![feature(macro_rules)]

extern crate libc;

macro_rules! cs {
    ($ex:expr) => {{
        (concat!($ex, "\0")).as_ptr() as *const libc::c_char
    }}
}

// does not compile!
//static cstring2: *const libc::c_char = cs!("hello 3");

fn main() {
    let cstring1: *const libc::c_char = cs!("hello 2");
    unsafe { 
        libc::puts(cs!("hello world"));
        libc::puts(cstring1);
    }
}
