use std::iter::AdditiveIterator;
use std::iter::Unfold;

fn main() {
  let sum = fibonacci()
    .take_while(|&e| e < 4_000_000)
    .filter(is_even)
    .sum();
  println!("{}", sum);
}

fn fibonacci() -> Unfold<uint, (uint, uint)> {
  Unfold::new((1, 2), fib_next)
}

fn fib_next(st: &mut (uint, uint)) -> Option<uint> {
  let prev = st.val0();
  let next = st.val1();
  *st.mut0() = next;
  *st.mut1() = prev + next;
  Some (prev)
}

fn is_even(i: &uint) -> bool { i % 2 == 0 }
