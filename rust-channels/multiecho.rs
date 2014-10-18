#![feature(phase, slicing_syntax)]
#[phase(plugin, link)] extern crate log;

use std::str;
use std::io::{TcpListener, TcpStream};
use std::io::{Acceptor, Listener};

fn main() {
    let addr = "127.0.1.1";
    let port = 9100;
    let listener = TcpListener::bind(addr, port);
    info!("listening on {}:{}", addr, port);
    let mut acceptor = listener.listen();
    for stream in acceptor.incoming() {
        match stream {
            Err (e) => warn!("failed to accept connection: {}", e),
            Ok (stream) => spawn(proc() { handle_client(stream) })
        }
    }
    drop(acceptor);
}

const MAXBUF : uint = 1024;

fn handle_client(mut stream: TcpStream) {
    info!("accepted connection from {}", stream.peer_name().unwrap());
    let buf : &mut [u8] = &mut [0, ..MAXBUF];
    match stream.read(buf) {
        Err (e) => fail!("TCP read error: {}", e),
        Ok (len) => echo(stream, buf.slice_to(len))
    }
}

fn echo(mut stream: TcpStream, buf: &[u8]) {
    debug!("echoing {} to {}", str::from_utf8(buf).unwrap().trim(),
           stream.peer_name().unwrap());
    match stream.write(buf) {
        Err (e) => fail!("TCP write error: {}", e),
        Ok (()) => ()
    }
}
