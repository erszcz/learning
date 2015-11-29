extern crate xml;

use std::cell::RefCell;
use std::collections::VecDeque;
use std::io::{ Read, Result as IOResult, Write };
use std::rc::Rc;
use xml::EventReader;

#[derive(Clone)]
struct RingBuffer {
    deque: Rc<RefCell<VecDeque<u8>>>
}

impl RingBuffer {
    fn new() -> RingBuffer {
        RingBuffer { deque: Rc::new(RefCell::new(VecDeque::new())) }
    }
}

impl Read for RingBuffer {
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        let mut deque = self.deque.borrow_mut();
        let len = deque.len();
        for b1 in buf.iter_mut() {
            if let Some(b2) = deque.pop_front() {
                *b1 = b2;
            } else {
                break;
            }
        }
        Ok (len)
    }
}

impl Write for RingBuffer {

    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        let mut deque = self.deque.borrow_mut();
        let len = buf.len();
        deque.reserve(len);
        for b in buf.iter() {
            deque.push_back(*b);
        }
        Ok (len)
    }

    fn flush(&mut self) -> IOResult<()> { Ok (()) }

}

fn main() {
    let mut buf = RingBuffer::new();
    buf.write("<a>".as_bytes()).expect("write failed");
    let mut parser = EventReader::new(buf.clone());
    while let Ok (ev) = parser.next() {
        println!("{:?}", ev);
        if let xml::reader::XmlEvent::EndDocument = ev {
            break;
        }
    }
    buf.write("</a><b/>".as_bytes()).expect("write failed");
    let mut parser = EventReader::new(buf.clone());
    while let Ok (ev) = parser.next() {
        println!("{:?}", ev);
        if let xml::reader::XmlEvent::EndDocument = ev {
            break;
        }
    }
}
