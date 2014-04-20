fn main() {
  for i in range(0, 100) {
    let msg = match i {
      _ if i % 15 == 0 => ~"fizzbuzz",
      _ if i % 5 == 0  => ~"fizz",
      _ if i % 3 == 0  => ~"buzz",
      _                => format_args!(std::fmt::format, "{}", i)
    };
    println(msg)
  }
}
