# Learning Flux

These are examples of [Flux][gh:flux] from
[a Flux tutorial on InfluxData blog][flux-tutorial].
The easiest way to try Flux and run the examples is the [TICK sandbox][gh:sandbox].

[flux-tutorial]: https://www.influxdata.com/blog/influxdb-how-to-do-joins-math-across-measurements/
[gh:flux]: https://github.com/influxdata/flux
[gh:sandbox]: https://github.com/influxdata/sandbox

## Running the examples

The tutorial describes using Chronograf,
the InfluxData web dashboard for metrics exploration.
However, it seems to be excruciatingly slow, at least on my 2013 MacBook.

It's also possible to use `influx -type=flux` to get a CLI Flux shell.
This one seems somewhat broken, at least when run in a container, due to
fancy syntax completion ASCII menus drawn in the terminal.

The most robust way I've tried seems to be the batch mode:

```
influx -database 'telegraf' -type flux -execute "$(cat script.flux)"
```

Where `script.flux` contains the script to run:

```
httpd = from(bucket:"telegraf/autogen")
|> range(start: -1h)
|> filter(fn:(r) => r._measurement == "influxdb_httpd" and r._field == "writeReq")
|> limit(n:3)
httpd
```
