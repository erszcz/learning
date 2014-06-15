use std::iter::Unfold;

fn main() {
  for i in fibonacci().take(10) {
    print!("{} ", i)
  }
  println!("")
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
