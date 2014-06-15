use std::iter::AdditiveIterator;
use std::iter::Unfold;

fn main() {
  let sum = fibonacci()
    .take_while(|&e| e < 4_000_000)
    .filter(is_even)
    .sum();
  println!("{}", sum);
}

fn fibonacci() -> Unfold<uint, FibState> {
  let st = FibState{curr: 1, next: 2};
  Unfold::new(st, fib_next)
}

struct FibState {
  curr: uint,
  next: uint
}

fn fib_next(st: &mut FibState) -> Option<uint> {
  let prev = st.curr;
  st.curr = st.next;
  st.next = prev + st.next;
  Some (prev)
}

fn is_even(i: &uint) -> bool { i % 2 == 0 }
