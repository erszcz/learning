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
  range(1, max)
    .filter(|x| { is_divisible_by(x, 3) || is_divisible_by(x, 5) })
    .fold(0, |acc, x| { acc + x })
}

fn is_divisible_by(num: &int, divisor: int) -> bool {
  num % divisor == 0
}
