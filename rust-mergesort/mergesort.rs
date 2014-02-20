use std::vec;

fn mergesort(a: ~[int], l: uint, r: uint) -> ~[int] {
    //println!("a = {}\nl = {}, r = {}", a.slice(l,r+1).to_str(), l, r);
    if l >= r {
        return a;
    }
    let mut b = a;
    b = mergesort(b, l, (l+r) / 2);
    //println!("a = {}\nl = {}, r = {}", b.slice(l,r+1).to_str(), l, r);
    b = mergesort(b, (l+r) / 2+1, r);
    merge(b, l, (l+r) / 2, (l+r) / 2+1, r)
}

fn merge(a: ~[int],
         lstart: uint, lend: uint,
         rstart: uint, rend: uint) -> ~[int] {
    //println!("l = {} r = {} lstart = {}, lend = {}, rstart = {}, rend = {}",
    //         a.slice(lstart, lend+1).to_str(),
    //         a.slice(rstart, rend+1).to_str(),
    //         lstart, lend, rstart, rend);
    assert!(lend < a.len());
    assert!(rend < a.len());
    if lstart >= rend {
        return a;
    }
    let mut i = lstart;
    let mut j = rstart;
    let mut k = 0;
    let mut b: ~[int] = vec::from_elem(lend - lstart + rend - rstart + 2, 0);
    //println!("capacity = {}", b.capacity());
    while k < b.capacity() && i <= lend && j <= rend {
        if a[i] < a[j] {
            b[k] = a[i];
            i += 1;
        } else {
            b[k] = a[j];
            j += 1;
        }
        k += 1;
        //println!("k: {} i: {} j: {} {}", k, i, j, b.to_str());
    }
    if k < b.capacity() {
        while i <= lend {
            b[k] = a[i];
            k += 1;
            i += 1;
            //println!("dupa1");
        }
        while j <= rend {
            //println!("dupa2");
            b[k] = a[j];
            k += 1;
            j += 1;
            //println!("dupa3");
        }
        //println!("dupa4");
    }
    let mut c = a;
    i = lstart;
    k = 0;
    while k < b.capacity() && i <= rend {
        c[i] = b[k];
        i += 1;
        k += 1;
    }
    //println!("c: {}", c.to_str());
    //println!("b: {}", b.to_str());
    c
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
    v = mergesort(v, 0, 2);
    assert!(v == ~[1,2,3])
}

#[test]
fn sorts_unsorted() {
    let mut v = ~[3,1,2];
    v = mergesort(v, 0, 2);
    assert!(v == ~[1,2,3])
}
