extern crate nlp;
use nlp::distance::levenshtein;

fn main() {
    println!("distance: {:u}", levenshtein("asd", "qwe"));
}
