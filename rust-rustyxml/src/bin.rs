extern crate xml;

use xml::{ Event, Parser };

fn main() {
    let mut parser = Parser::new();
    parser.feed_str("<a href");
    parser.feed_str("='//example.com'/><b");
    while let Some (ev) = parser.next() {
        println!("{:?}", ev);
    }
    parser.feed_str("/>");
    while let Some (ev) = parser.next() {
        println!("{:?}", ev);
    }
}
