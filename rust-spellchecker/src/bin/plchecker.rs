#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate nlp;
extern crate time;

use std::io::BufferedReader;
use std::io::File;
use std::slice::Items;
use time::precise_time_ns;

use nlp::distance::levenshtein as distance;

static DATA_PATH : &'static str = "data/formy-short.utf8";

struct Dict {
    items: Vec<String>
}

impl Dict {

    fn from_file(path: Path) -> Dict {
        let mut file = BufferedReader::new(File::open(&path));
        let lines: Vec<String> = file.lines().map(|x| {
            x.unwrap().as_slice().trim().to_string()
        }).collect();
        Dict{items: lines}
    }

    fn contains(&self, word: &String) -> bool {
        self.items.iter().any(|other_word| *word == *other_word)
    }

    fn iter<'a>(&'a self) -> Items<'a, String> {
        self.items.iter()
    }

}

struct Word<'a> {
    word: String,
    dict: &'a Dict,

    // Word is the origin of the coordinate space.
    // Distances define how far other words of the dictionary
    // are in the 1 dimensional space.
    distances: Option<Vec<uint>>
}

impl<'a> Word<'a> {

    fn new(s: &str, dict: &'a Dict) -> Word<'a> {
        Word{word: s.to_string(),
             dict: dict,
             distances: None}
    }

    fn is_valid(&self) -> bool {
        self.dict.contains(&self.word)
    }

    fn best_corrections(self, n: uint) -> Vec<Correction> {
        let unwrapped_distances = self.distances.unwrap();
        let mut word_distances : Vec<(&String, &uint)> =
            self.dict.iter().zip(unwrapped_distances.iter()).collect();
        word_distances.sort_by(|&(_,a), &(_,b)| a.cmp(b));
        word_distances
            .iter()
            .map(|&(word, score)| Correction { word: word.to_string(),
                                               score: *score })
            .take(n).collect()
    }

    fn calculate_distances(&mut self) {
        if self.distances.is_some()
            { return }
        let distances =
            self.dict.iter().map(|other_word| {
                distance(self.word.as_slice(), other_word.as_slice())
            }).collect();
        self.distances = Some(distances)
    }

    fn mutations(&self) -> WordMutations {
        WordMutations { u16_word: self.word.to_utf16(),
                        depleted: false }
    }

}

//fn mutate_word<'a>(word: &'a String) -> Iterator<'a, String> {
//    let mutated = MutatedWord { u16_word: word.to_utf16() };
//    let v = vec!(String::from_utf16(mutated.u16_word.as_slice()).unwrap());
//    v.items()
//}

struct WordMutations {
    u16_word: Vec<u16>,
    depleted: bool
}

impl Iterator<String> for WordMutations {

    fn next(&mut self) -> Option<String> {
        if self.depleted
            { None }
        else {
            self.depleted = true;
            String::from_utf16(self.u16_word.as_slice())
        }
    }

}

//fn fibonacci() -> Unfold<uint, (uint, uint)> {
//  Unfold::new((1, 2), fib_next)
//}

//fn fib_next(st: &mut (uint, uint)) -> Option<uint> {
//  let prev = st.val0();
//  let next = st.val1();
//  *st.mut0() = next;
//  *st.mut1() = prev + next;
//  Some (prev)
//}

struct Correction {
    word: String,
    score: uint
}

fn main() {
    let dict = Dict::from_file(Path::new(DATA_PATH));
    let w = Word::new( std::os::args()[1].as_slice(), &dict );
    for mutation in w.mutations()
        { println!("{:s}", mutation.as_slice()); }
}

//fn main() {

//    info!("building the dictionary from {:s}", DATA_PATH);
//    let ns_build_start = precise_time_ns();

//    let dict = Dict::from_file(Path::new(DATA_PATH));

//    let ns_build_elapsed = precise_time_ns() - ns_build_start;
//    info!("built in {:u}ms", ns_build_elapsed / 1000 / 1000);

//    let mut to_check = Word::new( std::os::args()[1].as_slice(), &dict );
//    let ncorrections = 5u;
//    if to_check.is_valid()
//        { println!("ok!") }
//    else {
//        println!("not a valid word");

//        info!("checking for corrections");
//        let ns_check_start = precise_time_ns();

//        to_check.calculate_distances();

//        println!("did you mean?");
//        for correction in to_check.best_corrections(ncorrections).iter() {
//            println!("- {:s} ({:u})",
//                     correction.word.as_slice(),
//                     correction.score);
//        }

//        let ns_check_elapsed = precise_time_ns() - ns_check_start;
//        info!("checking for corrections took {:u}ms",
//              ns_check_elapsed / 1000 / 1000);
//    }
//}
