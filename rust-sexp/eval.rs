#![feature(phase, globs)]
#[phase(plugin, link)] extern crate log;

use std::collections::HashMap;
use std::gc::Gc;
use sexp::SExp;

mod sexp;

// For now: evaluate a pure expression.
fn eval(sexp: SExp, _env: Env) {
  //match sexp {
  //}
}
