use futures::executor::block_on;

async fn hello() {
    println!("Hello");
}

async fn world() {
    println!("world!");
}

async fn async_main() {
    let f1 = hello();
    let f2 = world();
    futures::join!(f1, f2);
}

fn main() {
    for _ in 1..11 {
        let f = async_main();
        block_on(f);
    }
}
