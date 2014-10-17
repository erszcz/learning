#![feature(phase)]
#[phase(plugin, link)] extern crate log;
extern crate nlp;
extern crate time;

use std::io::BufferedReader;
use std::io::File;
use std::iter::{Chain, Unfold};
use std::slice::Items;
use time::precise_time_ns;

use nlp::distance::levenshtein as distance;

static DATA_PATH : &'static str = "data/formy0.125.utf8";

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

    fn contains(&self, word: &str) -> bool {
        self.items.iter().any(|other_word| *word == other_word.as_slice())
    }

    fn iter<'a>(&'a self) -> Items<'a, String> {
        self.items.iter()
    }

}

struct Word<'a> {
    // Dictionary to spellcheck against.
    dict: &'a Dict,

    // Word to spellcheck.
    word: String,

    // Candidate corrections.
    candidates: Option<Vec<String>>,

    // Word is the origin of the coordinate space.
    // Distances define how far the propositions are in 1D space.
    distances: Option<Vec<uint>>
}

impl<'a> Word<'a> {

    fn new(s: &str, dict: &'a Dict) -> Word<'a> {
        Word{word: s.to_string(),
             dict: dict,
             candidates: None,
             distances: None}
    }

    fn is_valid(&self) -> bool {
        self.dict.contains(self.word.as_slice())
    }

    fn best_corrections(self, n: uint) -> Vec<Correction> {
        match (self.candidates, self.distances) {
            (None, None) => fail!(),
            (Some (_), None) => fail!(),
            (None, Some (_)) => fail!(),
            (Some (ref candidates), Some (ref distances)) => {
                let mut scored : Vec<(&String,&uint)> =
                    candidates.iter().zip(distances.iter()).collect();
                scored.sort_by(|&(_,a), &(_,b)| a.cmp(b));
                scored.iter()
                    .map(|&(word, score)| Correction { word: word.to_string(),
                                                       score: *score })
                    .take(n).collect()
            }
        }
    }

    fn generate_candidates(&mut self) {
        if self.candidates.is_some()
            { return }
        let dict = self.dict;
        let is_valid = |word: &String| dict.contains(word.as_slice());
        let candidates : Vec<String> = self.mutations().filter(is_valid).collect();
        debug!("#candidates = {}", candidates.len());
        self.candidates = Some (candidates);
    }

    fn calculate_distances(&mut self) {
        if self.distances.is_some()
            { return }
        match self.candidates {
            None => (),
            Some (ref candidates) => {
                let distances =
                    candidates.iter().map(|other_word| {
                        distance(self.word.as_slice(), other_word.as_slice())
                    }).collect();
                self.distances = Some (distances)
            }
        }
    }

    fn shorter(&self) -> ShorterWords {
        ShorterWords { word: self.word.as_slice().utf16_units().collect(),
                       idx: 0 }
    }

    fn longer(&self) -> LongerWords {
        LongerWords { word: self.word.as_slice().utf16_units().collect(),
                      idx: 0,
                      letters: alphabet()}
    }

    fn equal(&self) -> EqualWords {
        EqualWords { word: self.word.as_slice().utf16_units().collect(),
                     idx: 0,
                     letters: alphabet()}
    }

    fn mutations(&self) -> Chain<Chain<ShorterWords, LongerWords>, EqualWords> {
        self.shorter().chain(self.longer()).chain(self.equal())
    }

}

struct ShorterWords {
    word: Vec<u16>,
    idx: uint
}

impl Iterator<String> for ShorterWords {

    fn next(&mut self) -> Option<String> {
        if self.idx >= self.word.len()
            { return None }
        let i = self.idx;
        self.idx += 1;
        let prefix = self.word.iter().take(i).map(|c|*c);
        let suffix = self.word.slice_from(i+1).iter().map(|c|*c);
        let short : Vec<u16> = prefix.chain(suffix).collect();
        String::from_utf16(short.as_slice())
    }

}

fn alphabet<'a>() -> Unfold<'a, u16, (uint, Vec<u16>)> {
    let abc : Vec<u16> =
        "aąbcćdeęfghijklłmnńoópqrsśtuvwxyzźż".utf16_units().collect();
    Unfold::new((0, abc),
                |&(ref mut idx, ref abc)| {
                    if *idx >= abc.len()
                        { None }
                    else {
                        let i = *idx;
                        *idx += 1;
                        Some (abc[i])
                    }
                })
}

struct LongerWords<'a> {
    word: Vec<u16>,
    idx: uint,
    letters: Unfold<'a, u16, (uint, Vec<u16>)>
}

impl<'a> Iterator<String> for LongerWords<'a> {

    fn next(&mut self) -> Option<String> {
        match self.letters.next() {
            Some (l) => {
                embed(self.word.as_slice(), self.idx, self.idx,
                      std::iter::iterate(l, |e| e).take(1))
            }
            None => {
                self.idx += 1;
                if self.idx > self.word.len()
                    { return None }
                self.letters = alphabet();
                embed(self.word.as_slice(), self.idx, self.idx,
                      std::iter::iterate(self.letters.next().unwrap(),
                                         |e| e).take(1))
            }
        }
    }

}

struct EqualWords<'a> {
    word: Vec<u16>,
    idx: uint,
    letters: Unfold<'a, u16, (uint, Vec<u16>)>
}

impl<'a> Iterator<String> for EqualWords<'a> {

    fn next(&mut self) -> Option<String> {
        match self.letters.next() {
            Some (l) => {
                embed(self.word.as_slice(), self.idx, self.idx+1,
                      std::iter::iterate(l, |e| e).take(1))
            }
            None => {
                self.idx += 1;
                if self.idx >= self.word.len()
                    { return None }
                self.letters = alphabet();
                embed(self.word.as_slice(), self.idx, self.idx+1,
                      std::iter::iterate(self.letters.next().unwrap(),
                                         |e| e).take(1))
            }
        }
    }

}

fn embed<I: Iterator<u16>>(word: &[u16], from: uint, to: uint, infix: I)
        -> Option<String> {
    let prefix = word.iter().take(from).map(|c| *c);
    let suffix = word.slice_from(to).iter().map(|c| *c);
    let short : Vec<u16> = prefix.chain(infix).chain(suffix).collect();
    String::from_utf16(short.as_slice())
}

struct Correction {
    word: String,
    score: uint
}

fn main() {

    info!("building the dictionary from {:s}", DATA_PATH);
    let ns_build_start = precise_time_ns();

    let dict = Dict::from_file(Path::new(DATA_PATH));

    let ns_build_elapsed = precise_time_ns() - ns_build_start;
    info!("built in {:u}ms", ns_build_elapsed / 1000 / 1000);

    let mut to_check = Word::new( std::os::args()[1].as_slice(), &dict );
    let ncorrections = 5u;
    if to_check.is_valid()
        { println!("ok!") }
    else {
        println!("not a valid word");

        info!("checking for corrections");
        let ns_check_start = precise_time_ns();

        to_check.generate_candidates();
        to_check.calculate_distances();

        println!("did you mean?");
        for correction in to_check.best_corrections(ncorrections).iter() {
            println!("- {:s} ({:u})",
                     correction.word.as_slice(),
                     correction.score);
        }

        let ns_check_elapsed = precise_time_ns() - ns_check_start;
        info!("checking for corrections took {:u}ms",
              ns_check_elapsed / 1000 / 1000);
    }
}
