extern crate xml;

use std::io::{ BufRead };
use std::net::{ TcpListener, TcpStream };
use std::thread;
use xml::EventReader;
use xml::reader::XmlEvent;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:3456").unwrap();

    // accept connections and process them, spawning a new thread for each one
    for stream in listener.incoming() {
        match stream {
            Ok (stream) => {
                thread::spawn(move|| {
                    // connection succeeded
                    handle_client(stream)
                });
            }
            Err (e) => { /* connection failed */ }
        }
    }

    // close the socket server
    drop(listener);
}

fn handle_client(stream: TcpStream) {
    let mut parser = EventReader::new(stream);
    'done: while let Ok (ev) = parser.next() {
        match ev {
            e @ XmlEvent::EndDocument => {
                println!("{:?}", e);
                break 'done;
            },
            e => println!("{:?}", e)
        }
    }
    println!("done");
}
