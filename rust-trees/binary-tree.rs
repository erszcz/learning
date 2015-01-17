#![feature(box_syntax)]

trait TreeData : Clone + Ord {}

impl TreeData for i32 {}

#[derive(Clone)]
struct Tree<T: TreeData> {
    l    : Option<Box<Tree<T>>>,
    r    : Option<Box<Tree<T>>>,
    data : T
}

impl<T: TreeData> Tree<T> {

    fn new(data: T) -> Tree<T> { Tree { data: data, l: None, r: None } }

    fn to_vec(t: Tree<T>) -> Vec<T> {
        let mut v: Vec<T> = vec!();
        inorder_traversal(Some (box t), |&mut:node_data|{ v.push(node_data) });
        v
    }

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

fn search<'b, T: TreeData>(key: T, tree: &'b Option<Box<Tree<T>>>)
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

fn insert<T: TreeData>(key: T, tree: &mut Option<Box<Tree<T>>>)
{
    match *tree {
        None => {
            *tree = Some (box Tree::new(key));
            return;
        },
        Some (ref mut boxed) => {
            match key {
                _ if key < boxed.data => insert(key, &mut boxed.l),
                _ if key > boxed.data => insert(key, &mut boxed.r),
                _ => ()
            }
        }
    }
}

#[inline]
fn height<'b, T: TreeData>(tree: &'b Option<Box<Tree<T>>>)
    -> usize { height_r(tree, 0) - 1 }

fn height_r<'b, T: TreeData>(tree: &'b Option<Box<Tree<T>>>, acc: usize)
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
fn preorder_traversal<T: TreeData, F: FnMut(T)>
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
fn inorder_traversal <T: TreeData, F: FnMut(T)>
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

#[cfg(test)]
fn one_element_tree(data: i32) -> Tree<i32> {
    Tree { l: None, r: None, data: data }
}

#[test]
fn search_in_one_element_tree() {
    let needle = 123;
    let t = one_element_tree(needle);
    let res: i32 = *search(needle, &Some(box t)).unwrap();
    assert_eq!(needle, res);
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

#[cfg(test)]
fn search_in_two_level_deep_tree(expected: i32) {
    let l = Tree { l: None, r: None, data: 23 };
    let r = Tree { l: None, r: None, data: 456 };
    let root = Tree { l: Some (box l), r: Some (box r), data: 123 };
    let res = *search(expected, &Some(box root)).unwrap();
    assert_eq!(expected, res);
}

#[test]
fn search_nonexistent_in_tree() {
    let data = 123;
    let t = one_element_tree(data);
    assert!(search(456, &Some(box t)).is_none());
}

#[test]
fn height_of_one_element_tree() {
    let t = one_element_tree(123);
    assert_eq!(0, height(&Some(box t)));
}

#[cfg(test)]
fn two_element_bst<T: TreeData>(d1: T, d2: T) -> Tree<T> {
    let r = Tree { l: None, r: None, data: d2 };
    Tree { l: None, r: Some (box r), data: d1 }
}

#[test]
fn height_of_two_element_bst() {
    let t = two_element_bst(123, 456);
    assert_eq!(1, height(&Some(box t)));
}

#[cfg(test)]
fn three_element_bst<T: TreeData>(d1: T, d2: T, d3: T) -> Tree<T> {
    assert!(d1 < d2);
    assert!(d2 < d3);
    let l = Tree { l: None, r: None, data: d1 };
    let r = Tree { l: None, r: None, data: d3 };
    Tree { l: Some (box l), r: Some (box r), data: d2 }
}

#[cfg(test)]
fn four_element_bst<T: TreeData>(d1: T, d2: T, d3: T, d4: T) -> Tree<T> {
    assert!(d1 < d2);
    assert!(d2 < d3);
    assert!(d3 < d4);
    let lr = Tree { l: None, r: None, data: d2 };
    let l = Tree { l: None, r: Some (box lr), data: d1 };
    let r = Tree { l: None, r: None, data: d4 };
    Tree { l: Some (box l), r: Some (box r), data: d3 }
}

#[test]
fn preorder_traversal_3bst() {
    let t = three_element_bst(23, 123, 456);
    let mut v: Vec<i32> = vec!();
    preorder_traversal(Some (box t),
                       |&mut:node_data|{ print!("{} ", node_data);
                                         v.push(node_data) });
    assert_eq!(v, vec![123, 23, 456]);
}

#[test]
fn preorder_traversal_4bst() {
    let t = four_element_bst(2, 5, 7, 9);
    let mut v: Vec<i32> = vec!();
    preorder_traversal(Some (box t),
                       |&mut:node_data|{ print!("{} ", node_data);
                                         v.push(node_data) });
    assert_eq!(v, vec![7, 2, 5, 9]);
}

#[test]
fn inorder_traversal_3bst() {
    let t = three_element_bst(23, 123, 456);
    let mut v: Vec<i32> = vec!();
    inorder_traversal(Some (box t),
                      |&mut:node_data|{ print!("{} ", node_data);
                                        v.push(node_data) });
    assert_eq!(v, vec![23, 123, 456]);
}

#[test]
fn inorder_traversal_4bst() {
    let t = four_element_bst(2, 5, 7, 9);
    let mut v: Vec<i32> = vec!();
    inorder_traversal(Some (box t),
                      |&mut:node_data|{ print!("{} ", node_data);
                                        v.push(node_data) });
    assert_eq!(v, vec![2, 5, 7, 9]);
}

#[test]
fn build_1bst() {
    let t1 = one_element_tree(123);
    let mut maybe_tree = None;
    insert(123, &mut maybe_tree);
    let v1 = Tree::to_vec(t1);
    let v2 = Tree::to_vec(*maybe_tree.expect("empty tree"));
    assert_eq!(v1, v2);
}

#[test]
fn build_2bst() {
    let t1 = two_element_bst(123, 456);
    let mut maybe_tree = None;
    insert(123, &mut maybe_tree);
    insert(456, &mut maybe_tree);
    let v1 = Tree::to_vec(t1);
    let v2 = Tree::to_vec(*maybe_tree.expect("empty tree"));
    assert_eq!(v1, v2);
}

#[test]
fn build_3bst() {
    let t1 = three_element_bst(23, 123, 456);
    let mut maybe_tree = None;
    insert(123, &mut maybe_tree);
    insert(23, &mut maybe_tree);
    insert(456, &mut maybe_tree);
    let v1 = Tree::to_vec(t1);
    let v2 = Tree::to_vec(*maybe_tree.expect("empty tree"));
    assert_eq!(v1, v2);
}

#[test]
fn build_4bst() {
    let t1 = four_element_bst(2, 5, 7, 9);
    let mut maybe_tree = None;
    insert(7, &mut maybe_tree);
    insert(2, &mut maybe_tree);
    insert(9, &mut maybe_tree);
    insert(5, &mut maybe_tree);
    let v1 = Tree::to_vec(t1);
    let v2 = Tree::to_vec(*maybe_tree.expect("empty tree"));
    assert_eq!(v1, v2);
}
