#![feature(io)]

// Works with:
// rustc 1.0.0-nightly (e233987ce 2015-02-27) (built 2015-02-28)

use std::io::{ BufRead, BufReader };
use std::mem;
use std::ptr;

#[repr(C)]
#[packed]
struct Configuration {
   item1: u8,
   item2: u16,
   item3: i32,
   item4: i64
}

static CONFIG_DATA: &'static [u8] = &[
  0xfd, 0xb4, 0x50, 0x45, 0xcd, 0x3c, 0x15, 0x71, 0x3c, 0x87, 0xff, 0xe8,
  0x5d, 0x20, 0xe7, 0x5f, 0x38, 0x05, 0x4a, 0xc4, 0x58, 0x8f, 0xdc, 0x67,
  0x1d, 0xb4, 0x64, 0xf2, 0xc5, 0x2c, 0x15, 0xd8, 0x9a, 0xae, 0x23, 0x7d,
  0xce, 0x4b, 0xeb
];

fn main() {
    println!("a");
    let config_size = mem::size_of::<Configuration>();
    let mut buffer = BufReader::with_capacity(config_size, CONFIG_DATA);
    let mut config: Configuration = unsafe { mem::uninitialized() };
    println!("b");
    match buffer.fill_buf() {
        Err (e) => panic!(e),
        Ok (b) => unsafe {
            ptr::copy(&mut config as *mut Configuration as *mut u8,
                      b.as_ptr(), config_size);
        }
    }
    println!("c");
    println!("{:?}\n{:?}\n{:?}\n{:?}",
             config.item1, config.item2, config.item3, config.item4);
}
