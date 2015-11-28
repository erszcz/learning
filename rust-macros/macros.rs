#[derive(Debug)]
pub struct Atom {
    atom: String
}

mod atom {
    
    macro_rules! define { ($atom:ident) => {
        #[inline]
        pub fn $atom() -> super::Atom {
            super::Atom { atom: "asd".to_string() }
        }
    } }

    define! { asd }

}

fn f() -> Result<(), ()> {
    try!(Err(()))
}

macro_rules! nif_try {
    ($env:expr, $res:expr) => {
        match $res {
            Ok (term) => term,
            Err (e) => return enif_raise_exception($env, e)
        }
    }
};

fn main() {
    println!("{:?}", atom::asd());
}
