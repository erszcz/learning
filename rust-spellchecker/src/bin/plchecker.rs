#![feature(macro_rules, phase)]
#[phase(plugin, link)] extern crate log;
extern crate nlp;
extern crate time;

use std::io::BufferedReader;
use std::io::File;
use std::iter::{Chain, Unfold};
use std::slice::Items;
use time::precise_time_ns;

use nlp::distance::{damerau, levenshtein};

/// Create a `Dict` containing the arguments.
#[macro_export]
macro_rules! dict(
    ($($e:expr),*) => ({
        let _d : Dict = vec!($($e),*).iter().collect();
        _d
    })
)

static DATA_PATH : &'static str = "data/formy.utf8";

trait Spellchecker {

    fn is_valid(&self, word: &str) -> bool;

    fn check(&self, word: &str) -> SpellcheckResult;

}

#[deriving(PartialEq, Eq, Show)]
enum SpellcheckResult<'a> {
    Valid,
    Invalid (Vec<Correction>),
    Error (&'a str)
}

struct PLChecker {
    dict: Dict,
    distance: fn (word1: &str, word2: &str) -> uint,
    ncorrections: uint
}

impl Spellchecker for PLChecker {

    fn is_valid(&self, word: &str) -> bool { self.dict.contains(word) }

    fn check(&self, word: &str) -> SpellcheckResult {
        if self.is_valid(word)
            { return Valid }
        //let candidates = generate_corrections(word, &self.dict);
        let candidates = all_words_as_corrections(word, &self.dict);
        debug!("candidates: {}", candidates);
        let scores = score_corrections(word, &candidates, self.distance);
        debug!("scores: {}", scores);
        Invalid (get_best_corrections(&candidates, &scores, self.ncorrections))
    }

}

fn all_words_as_corrections<'b>(_: &str, dict: &'b Dict) -> Vec<&'b str> {
    let corrections : Vec<&'b str> =
        dict.items.iter().map(|item| item.as_slice()).collect();
    corrections
}

fn generate_corrections<'b>(word: &str, dict: &'b Dict) -> Vec<&'b str> {
    let is_valid = |word: &String| dict.contains(word.as_slice());
    let candidates : Vec<&'b str> =
        Word { word: word.to_string() }
            .mutations()
            .map(|w| dict.intern(w.as_slice()))
            .filter(|opt| opt.is_some())
            .map(|opt| opt.unwrap()).collect();
    debug!("#candidates = {}", candidates.len());
    candidates
}

fn score_corrections(word: &str, candidates: &Vec<&str>,
                     distance: fn (&str, &str) -> uint) -> Vec<uint> {
    let scores : Vec<uint> = candidates.iter().map(|other_word| {
        distance(word, other_word.as_slice())
    }).collect();
    scores
}

fn get_best_corrections(candidates: &Vec<&str>,
                        distances: &Vec<uint>,
                        ncorrections: uint) -> Vec<Correction> {
    let mut scored : Vec<(&&str, &uint)> =
        candidates.iter().zip(distances.iter()).collect();
    scored.sort_by(|&(_,a), &(_,b)| a.cmp(b));
    scored.iter()
          .map(|&(word, score)|
               Correction { word: word.to_string(), score: *score })
          .take(ncorrections).collect()
}

#[test]
fn is_valid_test() {
    let d = dict!("asd");
    let c = PLChecker { dict: d, distance: levenshtein, ncorrections: 0u };
    assert!(c.is_valid("asd"));
    assert!(! c.is_valid("qwe"));
}

#[test]
fn check_valid_test() {
    let d = dict!("asd");
    let c = PLChecker { dict: d, distance: levenshtein, ncorrections: 0u };
    assert!(Valid == c.check("asd"));
}

#[test]
fn check_invalid_test() {
    let d = dict!("asd", "asf");
    let c = PLChecker { dict: d, distance: levenshtein, ncorrections: 2u };
    let example = vec!(Correction { word: "asd".to_string(), score: 100 },
                       Correction { word: "asf".to_string(), score: 100 });
    let real = c.check("ase");
    println!("spellcheck result: {}", real);
    assert!(Invalid (example) == real);
}

struct Dict {
    items: Vec<String>
}

impl Dict {

    fn from_file(path: Path) -> Dict {
        let mut file = BufferedReader::new(File::open(&path));
        let lines: Vec<String> = file.lines().map(|x| {
            match x {
                Err (reason) => fail!("{}", reason),
                Ok (line) => line.as_slice().trim().to_string()
            }
        }).collect();
        Dict{items: lines}
    }

    fn contains(&self, word: &str) -> bool {
        self.items.iter().any(|other_word| *word == other_word.as_slice())
    }

    // Take a &str, find an equivalent in the dictionary
    // and return a &str to the buffer in the dictionary.
    // This allows for reusing text data from the dictionary
    // instead of keeping multiple copies of the same string.
    fn intern(&self, word: &str) -> Option<&str> {
        let maybe_pos =
            self.items.iter().position(|item| item.as_slice() == word);
        match maybe_pos {
            None => None,
            Some (pos) => Some ( self.items[pos].as_slice() )
        }
    }

}

impl<'a, 'b> FromIterator<&'a &'b str> for Dict {
    fn from_iter<T: Iterator<&'a &'b str>>(i: T) -> Dict {
        Dict { items :i.map(|word| word.to_string()).collect() }
    }
}

#[test]
fn dict_from_iterator_test() {
    let _ : Dict = vec!("asd", "qwe").iter().collect();
}

#[test]
fn dict_macro_test() {
    let _ = dict!("asd", "qwe");
}

struct Word<'a> {
    // Word to spellcheck.
    word: String,
}

impl<'a> Word<'a> {

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
                // unwrap is safe here, as we just created a new alphabet
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
                // unwrap is safe here, as we just created a new alphabet
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

#[deriving(PartialEq, Eq, Show)]
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

    let spellchecker = PLChecker { dict: dict,
                                   distance: damerau,
                                   ncorrections: 15u };

    info!("checking for corrections");
    let ns_check_start = precise_time_ns();

    let to_check = std::os::args()[1].clone();
    match spellchecker.check(to_check.as_slice()) {
        Valid => println!("ok!"),
        Error (reason) => fail!("{}", reason),
        Invalid (corrections) => {
            println!("not a valid word");
            println!("did you mean?");
            for correction in corrections.iter() {
                println!("- {:s} ({:u})",
                         correction.word.as_slice(),
                         correction.score);
            }
        }
    }

    let ns_check_elapsed = precise_time_ns() - ns_check_start;
    info!("checking for corrections took {:u}ms",
          ns_check_elapsed / 1000 / 1000);

}
