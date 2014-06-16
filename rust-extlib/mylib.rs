#![crate_type = "lib"]
#![no_std]

struct RustObject {
    a: i32
}

#[no_mangle]
pub extern fn callback(target: *mut RustObject, a: i32) {
    // Update the value in RustObject with the value received from the callback
    unsafe { (*target).a = a; }
}
