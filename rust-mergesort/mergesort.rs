use std::vec;

fn mergesort(a: &mut [int], l: uint, r: uint) {
    if l >= r {
        return;
    }
    mergesort(a, l, (l+r) / 2);
    mergesort(a, (l+r) / 2+1, r);
    merge(a, l, (l+r) / 2, (l+r) / 2+1, r);
}

fn merge(a: &mut [int],
         lstart: uint, lend: uint,
         rstart: uint, rend: uint)
{
    assert!(lend < a.len());
    assert!(rend < a.len());
    if lstart >= rend {
        return;
    }
    let mut i = lstart;
    let mut j = rstart;
    let mut b: ~[int] = vec::with_capacity(lend - lstart + rend - rstart + 2);
    while i <= lend && j <= rend {
        if a[i] < a[j] {
            b.push(a[i]);
            i += 1;
        } else {
            b.push(a[j]);
            j += 1;
        }
    }
    assert!(b.capacity() - b.len() == lend - i + rend - j + 2);
    b = vec::append(b, a.slice(i, lend+1));
    b = vec::append(b, a.slice(j, rend+1));
    /* At this moment we must've copied all elements from the left and right
     * subvectors. */
    assert!(b.len() == b.capacity());
    let len = b.len();
    a.mut_slice(lstart, rend+1).move_from(b, 0, len);
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
    mergesort(v, 0, 2);
    assert!(v == ~[1,2,3])
}

#[test]
fn sorts_unsorted() {
    let mut v = ~[3,1,2];
    mergesort(v, 0, 2);
    assert!(v == ~[1,2,3])
}
