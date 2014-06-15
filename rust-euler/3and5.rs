fn main() {
  let args = std::os::args();
  if args.len() < 2 { fail!("specify natural range maximum") }
  let max = match from_str::<int>(args[1]) {
    Some (x) if x > 0 => x,
    None              => fail!("not an integer"),
    _                 => fail!("not a natural number")
  };
  println!("{}", sum_of_divisibles(max))
}

fn sum_of_divisibles(max: int) -> int {
  let mut sum = 0;
  for i in range(1, max) {
    if is_divisible_by(i, 3) || is_divisible_by(i, 5) { sum += i; }
  }
  sum
}

fn is_divisible_by(num: int, divisor: int) -> bool {
  num % divisor == 0
}
