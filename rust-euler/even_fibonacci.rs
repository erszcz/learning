fn main() {
  let mut fib = fib_new();
  for _ in range(1, 10) {
    print!("{} ", fib_next(&mut fib))
  }
  println!("")
}

struct FibState {
  curr: uint,
  next: uint
}

fn fib_new() -> FibState {
  FibState{
    curr: 1u,
    next: 1u
  }
}

fn fib_next(st: &mut FibState) -> uint {
  let prev = st.curr;
  st.curr = st.next;
  st.next = prev + st.next;
  prev
}

//fn fibonacci(n: uint) -> std::iter::Unfold<uint> {
//  std::iter::Unfold::new(0u, |&mut s| s+1)
//}
