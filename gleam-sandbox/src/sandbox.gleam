import gleam/io
import gleam/erlang/process

pub fn main() {
  io.println("Hello from sandbox!")
  proc_spawn()
}

pub fn proc_spawn() {
  let self = process.self()
  io.debug(#("self", self))
  let child = fn() {
    let self = process.self()
    io.debug(#("child", self))
  }
  |> process.start(False)
  io.debug(#("child from parent's view", child))
}
