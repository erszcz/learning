# How to make `fprof` output into something readable?

These hints are shamelessly stolen from:

- http://carbonshaft.blogspot.co.uk/2011/11/erlang-using-fprof-profiler-to-drive.html
- http://blog.equanimity.nl/blog/2013/04/24/fprof-kcachegrind/

What will be needed?

    $ brew install qcachegrind graphviz
    $ cd ~/bin
    $ wget https://raw.github.com/isacssouza/erlgrind/master/src/erlgrind
    $ chmod +x erlgrind

Given the files in this directory, there's no much else to do:

    $ erl
    1> c(string_lists).
    2> fprof:apply(string_lists, accumulator_join_test, []).
    3> fprof:profile().
    4> fprof:analyse({dest, "outfile.fprof"}).
    ^C^C
    $ erlgrind outfile.fprof outfile.cgrind
    $ qcachegrind outfile.cgrind

It's also possible to trace everything that happens in the system between
two distinct points in time:

    > fprof:trace([start, file]).
    %% Code to profile
    > fprof:trace(stop).
