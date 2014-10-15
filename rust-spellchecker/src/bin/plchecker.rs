extern crate nlp;

use std::io::BufferedReader;
use std::io::File;
use std::slice::Items;

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

}

struct Correction {
    word: String,
    score: uint
}

fn main() {
    let dict = Dict::new(DATA_PATH);
    let mut to_check = Word::new( std::os::args()[1].as_slice(), &dict );
    let ncorrections = 5u;
    if to_check.is_valid()
        { println!("ok!") }
    else {
        println!("not a valid word");
        to_check.calculate_distances();
        println!("did you mean?");
        for correction in to_check.best_corrections(ncorrections).iter() {
            println!("- {:s} ({:u})",
                     correction.word.as_slice(),
                     correction.score);
        }
    }
}
