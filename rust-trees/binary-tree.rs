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

#[inline]
fn height<'b, T: Ord>(tree: &'b Option<Box<Tree<T>>>)
    -> usize { height_r(tree, 0) - 1 }

fn height_r<'b, T: Ord>(tree: &'b Option<Box<Tree<T>>>, acc: usize)
    -> usize
{
    match *tree {
        None => acc,
        Some (ref boxed) => {
            let lh = height_r(&boxed.l, acc);
            let rh = height_r(&boxed.r, acc);
            if lh > rh { lh + 1 }
            else       { rh + 1 }
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

#[test]
fn height_of_one_element_tree() {
    let t = Tree { l: None, r: None, data: 123 };
    assert_eq!(0, height(&Some(box t)));
}

#[test]
fn height_of_two_element_tree() {
    let r = Tree { l: None, r: None, data: 456i64 };
    let root = Tree { l: None, r: Some (box r), data: 123i64 };
    assert_eq!(1, height(&Some(box root)));
}
