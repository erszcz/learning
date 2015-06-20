trait HasArea {
    fn area(&self) -> f64;
}

struct Circle {
    c: (f64, f64),
    r: f64
}

impl HasArea for Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.r * self.r)
    }
}

struct Square {
    a: f64
}

impl HasArea for Square {
    fn area(&self) -> f64 {
        self.a * self.a
    }
}

fn area_1<Shape: HasArea>(s: &Shape) -> f64 {
    s.area()
}

fn area_2(s: &HasArea) -> f64 {
    s.area()
}

fn main() {
    let circle = Circle { c: (2.0,2.0), r: 3.0 };
    let square = Square { a: 6.0 };
    println!("circle area = {}", area_1(&circle));
    println!("square area = {}", area_1(&square));
    println!("circle area = {}", area_2(&circle));
    println!("square area = {}", area_2(&square));
}
