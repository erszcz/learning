#![feature(box_syntax)]
use std::fmt::String;

#[derive(Clone)]
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

// Perform a preorder traversal not using recursion.
fn preorder_traversal<T: Ord+String+Clone, F: FnMut(T)>
                     (tree: Option<Box<Tree<T>>>, mut f: F) {
    if tree.is_none()
        { return }
    let mut stack = vec!();
    stack.push((*tree.unwrap()));
    while !stack.is_empty() {
        let this = stack.pop().expect("popped from an empty stack");
        f(this.data);
        if this.r.is_some() { stack.push(*this.r.unwrap()) }
        if this.l.is_some() { stack.push(*this.l.unwrap()) }
    }
}

// Perform an inorder traversal not using recursion.
fn inorder_traversal <T: Ord+String+Clone, F: FnMut(T)>
                     (tree: Option<Box<Tree<T>>>, mut f: F) {
    enum Visit { Descent, Ascent }
    if tree.is_none()
        { return }
    let mut stack = vec!();
    stack.push( (Visit::Descent, *tree.unwrap()) );
    while !stack.is_empty() {
        let (visit, this) = stack.pop().expect("popped from an empty stack");
        match visit {
            Visit::Descent => {
                stack.push( (Visit::Ascent, this.clone()) );
                if this.l.is_some()
                    { stack.push( (Visit::Descent, *this.l.unwrap()) ) }
            },
            Visit::Ascent => {
                f(this.data);
                if this.r.is_some() {
                    stack.push( (Visit::Descent, *this.r.unwrap()) );
                }
            }
        }
    }
}

#[test]
fn search_in_one_element_tree() {
    let t = Tree { l: None, r: None, data: 123 };
    let res: i32 = *search(123, &Some(box t)).unwrap();
    assert_eq!(123, res);
}

#[test]
fn search_left_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(23);
}

#[test]
fn search_right_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(456);
}

#[test]
fn search_root_in_two_level_deep_tree() {
    search_in_two_level_deep_tree(123);
}

#[test]
fn search_nonexistent_in_tree() {
    let t = Tree { l: None, r: None, data: 123 };
    assert!(search(456, &Some(box t)).is_none());
}

#[cfg(test)]
fn search_in_two_level_deep_tree(expected: i32) {
    let l = Tree { l: None, r: None, data: 23 };
    let r = Tree { l: None, r: None, data: 456 };
    let root = Tree { l: Some (box l), r: Some (box r), data: 123 };
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
    let r = Tree { l: None, r: None, data: 456 };
    let root = Tree { l: None, r: Some (box r), data: 123 };
    assert_eq!(1, height(&Some(box root)));
}

#[test]
fn preorder_traversal_test() {
    let l = Tree { l: None, r: None, data: 23 };
    let r = Tree { l: None, r: None, data: 456 };
    let root = Tree { l: Some (box l), r: Some (box r), data: 123 };
    let mut v: Vec<i32> = vec!();
    preorder_traversal(Some (box root),
                       |&mut:node_data|{ print!("{} ", node_data);
                                         v.push(node_data) });
    assert_eq!(v, vec![123, 23, 456]);
}

#[test]
fn inorder_traversal_test() {
    let l = Tree { l: None, r: None, data: 23 };
    let r = Tree { l: None, r: None, data: 456 };
    let root = Tree { l: Some (box l), r: Some (box r), data: 123 };
    let mut v: Vec<i32> = vec!();
    inorder_traversal(Some (box root),
                      |&mut:node_data|{ print!("{} ", node_data);
                                        v.push(node_data) });
    assert_eq!(v, vec![23, 123, 456]);
}
