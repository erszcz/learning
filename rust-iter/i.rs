trait Iterable {
    fn iter(&self) -> Iterator<Item=IterableItem>;
}

#[derive(Show)]
struct IterableItem { item : usize }

struct A;

struct AIter { u : usize }

impl Iterator for AIter {
    type Item = IterableItem;
    fn next(&mut self) -> Option<IterableItem> {
        match self.u {
            a@0 | a@1 | a@2 => {
                self.u = a + 1;
                Some (IterableItem { item: a })
            }
            _ => None
        }
    }
}

struct B;

struct BIter { u : usize }

impl<A> Iterator for Box<Iterator<Item=A>+'static> {
    fn next(&mut self) -> Option<A> { self.next() }
}

//trait BoxIterator<A>: Iterator<Item=A> {}
//impl<A, I: Iterator<Item=A>> BoxIterator<A> for I {}

//impl Iterable for A {
//    fn iter(&self) -> Box<AIter> { Box::new(AIter { u: 0 }) }
//}

impl Iterable for B {
    fn iter(&self) -> Box<BIter> { Box::new(BIter { u: 0 }) }
}

fn main() {
    let a = A;
    let b = B;
    for x in a.iter() {
        println!("{:?}", x);
    }
    for x in b.iter() {
        println!("{:?}", x);
    }
}
