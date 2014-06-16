struct RustObject {
    a: i32
}

extern fn callback(target: *mut RustObject, a: i32) {
    println!("I'm called from C with value {0}", a);
    // Update the value in RustObject with the value received from the callback
    unsafe { (*target).a = a; }
}

#[link(name = "extlib")]
extern {
   fn register_callback(target: *mut RustObject, cb: extern "C" fn(*mut RustObject, i32)) -> i32;
   fn trigger_callback();
}

fn main() {
    // Create the object that will be referenced in the callback
    let rust_object = &RustObject{a: 5};

    println!("Value is now {}", rust_object.a);
    unsafe {
        let target_object : *mut RustObject = std::cast::transmute(rust_object);
        register_callback(target_object, callback);
        trigger_callback(); // Triggers the callback
    }
    println!("Value is now {}", rust_object.a);
}
