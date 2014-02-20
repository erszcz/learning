use std::vec;

fn mergesort(a: ~[int], l: uint, r: uint) -> ~[int] {
    if l >= r {
        return a;
    }
    let mut b = a;
    b = mergesort(b, l, (l+r) / 2);
    b = mergesort(b, (l+r) / 2+1, r);
    merge(b, l, (l+r) / 2, (l+r) / 2+1, r)
}

fn merge(a: ~[int],
         lstart: uint, lend: uint,
         rstart: uint, rend: uint) -> ~[int]
{
    assert!(lend < a.len());
    assert!(rend < a.len());
    if lstart >= rend {
        return a;
    }
    let mut i = lstart;
    let mut j = rstart;
    let mut k = 0;
    let mut b: ~[int] = vec::from_elem(lend - lstart + rend - rstart + 2, 0);
    while k < b.capacity() && i <= lend && j <= rend {
        if a[i] < a[j] {
            b[k] = a[i];
            i += 1;
        } else {
            b[k] = a[j];
            j += 1;
        }
        k += 1;
    }
    assert!(k <= b.capacity());
    assert!(b.capacity() - k == lend - i + rend - j + 2);
    while i <= lend {
        b[k] = a[i];
        k += 1;
        i += 1;
    }
    while j <= rend {
        b[k] = a[j];
        k += 1;
        j += 1;
    }
    /* At this moment we must've copied all elements from the left and right
     * subvectors. */
    assert!(i-1 == lend);
    assert!(j-1 == rend);
    let mut c = a;
    i = lstart;
    k = 0;
    while k < b.capacity() && i <= rend {
        c[i] = b[k];
        i += 1;
        k += 1;
    }
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
