#![feature(box_syntax)]

struct Tree<T> {
    l    : Option<Box<Tree<T>>>,
    r    : Option<Box<Tree<T>>>,
    data : T
}

#[allow(unused_parens)]
impl<T: PartialEq> PartialEq for Tree<T> {
    fn eq(&self, other: &Tree<T>) -> bool {
        return (self.l == other.l &&
                self.r == other.r &&
                self.data == other.data);
    }
}

impl<T: Eq> Eq for Tree<T> {}

fn search<'b, T: Ord>(key: T, tree: &'b Option<Box<Tree<T>>>)
    -> Option<&'b T>
{
    let mut this: &'b Option<Box<Tree<T>>> = tree;
    loop {
        match *this {
            None => return None,
            Some (ref boxed) => {
                match key {
                    _ if key == boxed.data => return Some (&boxed.data),
                    _ if key  < boxed.data => this = &boxed.l,
                    _ if key  > boxed.data => this = &boxed.r,
                    _ => return None
                }
            }
        }
    }
}

#[test]
fn search_in_one_element_tree() {
    let t = Tree { l: None, r: None, data: 123 };
    let res: i64 = *search(123, &Some(box t)).unwrap();
    assert_eq!(123, res);
}

#[test]
fn search_left_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(23i64);
}

#[test]
fn search_right_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(456i64);
}

#[test]
fn search_root_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(123i64);
}

#[test]
fn search_nonexistent_in_tree() {
    let t = Tree { l: None, r: None, data: 123 };
    assert!(search(456, &Some(box t)).is_none());
}

#[cfg(test)]
fn search_in_two_level_deep_tree(expected: i64) {
    let l = Tree { l: None, r: None, data: 23i64 };
    let r = Tree { l: None, r: None, data: 456i64 };
    let root = Tree { l: Some (box l), r: Some (box r), data: 123i64 };
    let res = *search(expected, &Some(box root)).unwrap();
    assert_eq!(expected, res);
}
