# Rust lifetimes: Getting away with things that would be reckless in C++

This is a ripoff of [a blog post of the same title][1].
I wanted to illustrate the points stated in the post
with real, though simplistic, code examples.

[1]: http://www.randomhacks.net/2014/09/19/rust-lifetimes-reckless-cxx/

Run `valgrind ./reckless-buggy` to see the problem.
Run valgrind on `./reckless-correct` to see the difference.
Imagine, the problem might be hidden in a codebase thousands of lines long
in some flyweight pattern or string interning code.

Even if you rely on iterators, Boost or whatever, the issues still apply,
since the pointers are underneath the high-level abstractions.

Finally, the problem is not even possible in Rust - `rustc` won't compile
code written as the blog post suggests.
That's the power of linear types.
