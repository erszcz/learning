use std::cmp::min;
use std::vec;

pub fn f() -> f64 { 1.0 }

pub fn levenshtein(s: &str, t: &str) -> uint {
    3
}
//    let m = s.char_len();
//    let n = t.char_len();
//    let mut d: ~[~[uint]] = vec::from_elem(m+1, vec::from_elem(n+1, 0u));
//    for i in range(0u, m+1) { d[i][0] = i; }
//    for j in range(0u, n+1) { d[0][j] = j; }
//    for j in range(1, n+1) {
//        for i in range(1, m+1) {
//            if s[i-1] == t[j-1] {
//                // no operation required
//                d[i][j] = d[i-1][j-1];
//            }
//            else {
//                d[i][j] = min(min(// a deletion
//                                  d[i-1][j] + 1,
//                                  // an insertion
//                                  d[i][j-1] + 1),
//                                  // a substitution
//                                  d[i-1][j-1] + 1);
//            }
//        }
//    }
//    d[m][n]
//}

//#[test]
//fn basics() {
//    assert!(0 == levenshtein("aa", "aa"))
//    assert!(1 == levenshtein("aa", "ab"))
//}

//fn main() {
//    let args = std::os::args();
//    if args.len() < 3 {
//        println!("usage: {} string1 string2", args[0]);
//        return;
//    }
//    println!("{}", levenshtein(args[1], args[2]));
//}
