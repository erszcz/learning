extern crate bufstream;

use bufstream::BufStream;
use std::io::{ BufRead };
use std::net::{ TcpListener, TcpStream };
use std::thread;

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
    println!("accepted connection");
    let buffered = BufStream::new(stream);
    for l in buffered.lines() {
        if let Ok (text) = l {
            println!("{:?}", text);
        }
    }
}
