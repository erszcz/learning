fn main() {
    let reg: [u8, .. 3] = [1,2,3];
    for &v in reg.iter() {
        println!("{} ", v);
    }
}
