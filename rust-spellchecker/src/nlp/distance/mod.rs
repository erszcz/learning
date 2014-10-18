use std::cmp::min;

fn cost(factor: f64) -> uint { (factor * 100.0) as uint }

pub fn levenshtein(s: &str, t: &str) -> uint {
    // Prepare empty 2D vector.
    let m = s.char_len();
    let n = t.char_len();
    let mut d: Vec<Vec<uint>> = Vec::from_elem(m+1, Vec::from_elem(n+1, 0u));
    // Set the boundary condition.
    for i in range(0u, m+1) { *d.get_mut(i).get_mut(0) = cost(i as f64); }
    for j in range(0u, n+1) { *d.get_mut(0).get_mut(j) = cost(j as f64); }
    // Calculate the distance.
    for (mut j, q) in t.chars().enumerate() {
        j += 1;
        for (mut i, p) in s.chars().enumerate() {
            i += 1;
            if p == q {
                // no operation required
                *d.get_mut(i).get_mut(j) = d[i-1][j-1];
            } else {
                *d.get_mut(i).get_mut(j) = min(min(// a deletion
                                                   d[i-1][j] + cost(1.0),
                                                   // an insertion
                                                   d[i][j-1] + cost(1.0)),
                                                   // a substitution
                                                   d[i-1][j-1] + cost(1.));
            }
        }
    }
    d[m][n]
}

#[test]
fn levenshtein_test() {
    assert!(cost(1.) == levenshtein("asd", "ase"));
    assert!(cost(1.) == levenshtein("asd", "ad"));
    // transposition is two basic operations
    assert!(cost(2.) == levenshtein("asd", "ads"));
}

pub fn damerau(s: &str, t: &str) -> uint {
    // Prepare empty 2D vector.
    let m = s.char_len();
    let n = t.char_len();
    let mut d: Vec<Vec<uint>> = Vec::from_elem(m+1, Vec::from_elem(n+1, 0u));
    // Set the boundary condition.
    for i in range(0u, m+1) { *d.get_mut(i).get_mut(0) = cost(i as f64); }
    for j in range(0u, n+1) { *d.get_mut(0).get_mut(j) = cost(j as f64); }
    // Remember two previous characters.
    let mut p_prev = s.char_at(0);
    let mut q_prev = t.char_at(0);
    // Calculate the distance.
    for (mut j, q) in t.chars().enumerate() {
        j += 1;
        for (mut i, p) in s.chars().enumerate() {
            i += 1;
            if p == q {
                // no operation required
                *d.get_mut(i).get_mut(j) = d[i-1][j-1];
            } else {
                let c = if p == q { cost(0.0) }
                        else      { cost(0.5) };
                *d.get_mut(i).get_mut(j) = min(min(// a deletion
                                                   d[i-1][j] + cost(1.0),
                                                   // an insertion
                                                   d[i][j-1] + cost(1.0)),
                                                   // a substitution
                                                   d[i-1][j-1] + cost(1.0));
                if i > 1 && j > 1 && p == q_prev && p_prev == q {
                    *d.get_mut(i).get_mut(j) = min(d[i][j],
                                                   // transposition
                                                   d[i-2][j-2] + c)
                }
            }
            p_prev = p;
        }
        q_prev = q;
    }
    d[m][n]
}

#[test]
fn damerau_test() {
    assert!(cost(1.0) == damerau("asd", "ase"));
    assert!(cost(1.0) == damerau("asd", "as"));
    // transposition is a basic operation
    assert!(cost(0.5) == damerau("asd", "ads"));
}
