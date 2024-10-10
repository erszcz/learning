use rustler::thread::{spawn, ThreadSpawner};
use rustler::types::{Atom, atom::ok};
use rustler::{Env, NifMap, NifTuple, NifUnitEnum, NifTaggedEnum, NifUntaggedEnum};
use std::{thread, time::Duration};

// See Derive Macros docs at https://docs.rs/rustler/0.26.0/rustler/index.html#derives

#[derive(NifMap)]
struct MyMap {
    lhs: i32,
    rhs: i32,
}

#[derive(NifTuple)]
struct MyTuple {
    lhs: i32,
    rhs: i32,
}

#[derive(NifUnitEnum)]
enum UnitEnum {
    FooBar,
    Baz,
}

#[derive(NifTaggedEnum)]
enum TaggedEnum {
    Foo,
    Bar(String),
    Baz{ a: i32, b: i32 },
}

#[derive(NifUntaggedEnum)]
enum UntaggedEnum {
    Foo(u32),
    Bar(String),
}

#[derive(NifTaggedEnum)]
enum Message {
    Tick(i32),
    Done
}

#[rustler::nif(name = "add")]
fn add_nif(a: i64, b: i64) -> i64 {
    add(a, b)
}

fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[rustler::nif(name = "my_map")]
fn my_map_nif() -> MyMap {
    my_map()
}

#[rustler::nif]
fn my_maps() -> Vec<MyMap> {
    vec![ my_map(), my_map()]
}

fn my_map() -> MyMap {
    MyMap { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn my_tuple() -> MyTuple {
    MyTuple { lhs: 33, rhs: 21 }
}

#[rustler::nif]
fn unit_enum_echo(unit_enum: UnitEnum) -> UnitEnum {
    unit_enum
}

#[rustler::nif]
fn tagged_enum_echo(tagged_enum: TaggedEnum) -> TaggedEnum {
    tagged_enum
}

#[rustler::nif]
fn untagged_enum_echo(untagged_enum: UntaggedEnum) -> UntaggedEnum {
    untagged_enum
}

#[rustler::nif]
fn start_timer(env: Env) -> Atom {
    let process = env.pid();
    spawn::<ThreadSpawner, _>(env, move |env| {
        let mut n = 1;
        let one_second = Duration::from_millis(1000);
        while n <= 3600 {
            let _ = env.send(&process, Message::Tick(n));
            n += 1;
            thread::sleep(one_second);
        };
        Atom::from_str(env, "done").expect("can't create atom").to_term(env)
    });
    ok()
}

rustler::init!("timer_nif");

#[cfg(test)]
mod tests {
    use crate::add;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
