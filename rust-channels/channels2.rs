struct Stop;

fn main() {
    for i in range(0u, 100) {
        let (tx,rx) = channel();
        spawn(proc() {
            println!("\tchild {}", i);
            tx.send(Stop);
        });
        println!("parent {}", i);
        rx.recv();
    }
}
