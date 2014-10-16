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
                embed(&self.word, self.idx, self.idx,
                      std::iter::iterate(l, |e| e).take(1))
            }
            None => {
                self.idx += 1;
                if self.idx > self.word.len()
                    { return None }
                self.letters = alphabet();
                embed(&self.word, self.idx, self.idx,
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
                embed(&self.word, self.idx, self.idx+1,
                      std::iter::iterate(l, |e| e).take(1))
            }
            None => {
                self.idx += 1;
                if self.idx >= self.word.len()
                    { return None }
                self.letters = alphabet();
                embed(&self.word, self.idx, self.idx+1,
                      std::iter::iterate(self.letters.next().unwrap(),
                                         |e| e).take(1))
            }
        }
    }

}

fn embed<I: Iterator<u16>>(word: &Vec<u16>, from: uint, to: uint, infix: I)
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
    let dict = Dict::from_file(Path::new(DATA_PATH));
    let w = Word::new( std::os::args()[1].as_slice(), &dict );
    //println!("shorter words:");
    //for mutation in w.shorter()
    //    { println!("{:s}", mutation.as_slice()); }
    println!("longer words:");
    for mutation in w.longer()
        { println!("{:s}", mutation.as_slice()); }
    println!("equal words:");
    for mutation in w.equal()
        { println!("{:s}", mutation.as_slice()); }
}
