use std::vec;
use std::io::File;

fn mergesort<T: Clone + Ord>(a: &mut [T]) {
    let len = a.len();
    if len <= 1 {
        return;
    }
    mergesort(a.mut_slice(0, len / 2));
    mergesort(a.mut_slice(len / 2, len));
    merge(a.mut_slice(0, len));
}

fn merge<T: Clone + Ord>(a: &mut [T]) {
    let alen = a.len();
    if alen <= 1 {
        return;
    }
    let mut i = 0;
    let mut j = alen / 2;
    let mut b: ~[T] = vec::with_capacity(alen);
    while i < alen / 2 && j < alen {
        if a[i] < a[j] {
            b.push(a[i].clone());
            i += 1;
        } else {
            b.push(a[j].clone());
            j += 1;
        }
    }
    assert!(b.capacity() - b.len() == alen / 2 - i + alen - j);
    b = vec::append(b, a.slice(i, alen / 2));
    b = vec::append(b, a.slice(j, alen));
    /* At this moment we must've copied all elements from the left and right
     * subvectors. */
    assert!(b.len() == b.capacity());
    let blen = b.len();
    a.mut_slice(0, alen).move_from(b, 0, blen);
}

#[test]
fn referential_transparency() {
    let v1 = ~[1,2,3];
    let v2 = ~[1,2,3];
    assert!(v1 == v2)
}

#[test]
fn sorts_sorted() {
    let mut v = ~[1,2,3];
    mergesort(v);
    assert!(v == ~[1,2,3])
}

#[test]
fn sorts_unsorted() {
    let mut v = ~[3,1,2];
    mergesort(v);
    assert!(v == ~[1,2,3])
}

fn main() {
    let args = std::os::args();
    if args.len() < 2 {
        println!("usage: {} file-to-sort", args[0]);
        return;
    }
    let mut contents = File::open(&Path::new(args[1])).read_to_end();
    mergesort(contents);
    for &e in contents.iter() {
        print!("{} ", e);
    }
    println!("");
}
