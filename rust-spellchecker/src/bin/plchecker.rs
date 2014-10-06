extern crate nlp;

use std::io::BufferedReader;
use std::io::File;

use nlp::distance::levenshtein as distance;

static DATA_PATH : &'static str = "data/formy-short.utf8";

struct Dict {
    items: Vec<String>
}

impl Dict {

    fn new(p: &str) -> Dict {
        let path = Path::new(p);
        let mut file = BufferedReader::new(File::open(&path));
        let lines: Vec<String> = file.lines().map(|x| {
            x.unwrap().as_slice().trim().to_string()
        }).collect();
        Dict{items: lines}
    }

    fn contains(&self, word: &String) -> bool {
        self.items.iter().any(|other_word| *word == *other_word)
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

    fn new<'a>(s: &str, dict: &'a Dict) -> Word<'a> {
        Word{word: s.to_string(),
             dict: dict,
             distances: None}
    }

    fn is_valid(&self) -> bool {
        self.dict.contains(&self.word)
    }

    fn best_corrections(&self) -> Vec<String> {
        vec!()
    }

}

fn main() {
    let dict = Dict::new(DATA_PATH);
    let to_check = Word::new( std::os::args()[1].as_slice(), &dict );
    if to_check.is_valid() {
        println!("ok!")
    } else {
        println!("not a valid word")
    }
}
