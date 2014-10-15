use std::cmp::min;

pub fn levenshtein(s: &str, t: &str) -> uint {
    let m = s.char_len();
    let n = t.char_len();
    let mut d: Vec<Vec<uint>> = Vec::from_elem(m+1, Vec::from_elem(n+1, 0u));
    for i in range(0u, m+1) { *d.get_mut(i).get_mut(0) = i; }
    for j in range(0u, n+1) { *d.get_mut(0).get_mut(j) = j; }
    let mut j = 0;
    for q in t.chars() {
        j += 1;
        let mut i = 0;
        for p in s.chars() {
            i += 1;
            if p == q {
                // no operation required
                *d.get_mut(i).get_mut(j) = d[i-1][j-1];
            } else {
                *d.get_mut(i).get_mut(j) = min(min(// a deletion
                                                   d[i-1][j] + 1u,
                                                   // an insertion
                                                   d[i][j-1] + 1u),
                                                   // a substitution
                                                   d[i-1][j-1] + 1u);
            }
        }
    }
    d[m][n]
}
