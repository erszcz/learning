fn main() {
    for i in range(0u, 100) {
        spawn(proc() {
            println!("\tchild {}", i);
        });
        println!("parent {}", i);
    }
}
