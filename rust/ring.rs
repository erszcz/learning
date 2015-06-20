//fn main() {
//    let mut i: int = 0;
//    while i < 4 {
//        println(i.to_str());
//        i += 1;
//    }
//}

//fn main() {
//    let mut i = 0;
//    do 4.times {
//        println(fmt!("ala ma kota, a kot ma ale %?", i));
//        i += 1;
//    }
//}

//fn main() {
//    for i in range(0,3) {
//        println!("ala ma kota, a kot ma ale {}", i);
//    }
//    let rest = [3,4,5];
//    for &i in rest.iter() {
//        println(fmt!("ala ma kota, a kot ma ale %?", i));
//    }
//}

//fn main() {
//    let ns = std::vec::from_fn(4, |n| n);
//}

fn main() {
    let ntasks = 4;
    let conns: std::vec<(Port<int>, Chan<int>)> = std::vec::from_fn(ntasks, |_| stream());
}
