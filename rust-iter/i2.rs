struct A { i: isize }

impl A {
    fn iter(&self) -> Box<AIter> {
        Box::new(AIter { i: self.i })
    }
}

struct AIter { i: isize }

impl Iterator for AIter {
    type Item = isize;
    
    fn next(&mut self) -> Option<isize> {
        match self.i {
            a@0 | a@1 | a@2 => {
                self.i = a+1;
                Some (a)
            }
            _ => None
        }
    }
}

struct B { i: isize }

impl B {
    fn iter(&self) -> Box<BIter> {
        Box::new(BIter { i: self.i })
    }
}

struct BIter { i: isize }

impl Iterator for BIter {
    type Item = isize;
    
    fn next(&mut self) -> Option<isize> {
        match self.i {
            a@3 | a@4 | a@5 => {
                self.i = a+1;
                Some (a)
            }
            _ => None
        }
    }
}

fn main() {
    let a = A { i: 0 };
    let b = B { i: 3 };
    let args = std::os::args();
    let mut i: Box<Iterator<Item=isize>> =
        if args[1] == "a" {
            a.iter()
        } else if args[1] == "b" {
            b.iter()
        } else {
            println!("a or b?");
            exit(1);
        };
    for x in i {
        println!("{:?}", x);
    }
}
