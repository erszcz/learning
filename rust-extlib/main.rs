struct RustObject {
    a: i32,
    // other members
}

extern fn callback(target: *RustObject, a:i32) {
    println!("I'm called from C with value {0}", a);
    (*target).a = a; // Update the value in RustObject with the value received from the callback
}

#[link(name = "extlib")]
extern {
   fn register_callback(target: *RustObject, cb: extern "C" fn(*RustObject, i32)) -> i32;
   fn trigger_callback();
}

fn main() {
    // Create the object that will be referenced in the callback
    let rust_object = ~RustObject{a: 5, ...};

    unsafe {
        // Gets a raw pointer to the object
        let target_addr:*RustObject = ptr::to_unsafe_ptr(rust_object);
        register_callback(target_addr, callback);
        trigger_callback(); // Triggers the callback
    }
}
