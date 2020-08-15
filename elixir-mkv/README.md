# MKV


## Build

```
mix deps.get
mix deps.compile
mix compile
```


## Testing

```
mix test
```


## Manual testing

First, run the app - development mode is fine:

```
iex -S mix
```

Then, to test the REST endpoint store and retrieve a value with curl:

```
# store 'asd' under key 'a'
curl -X PUT -d $(echo asd | base64) http://localhost:4001/v1/kv/a
# retrieve value stored under 'a'
curl http://localhost:4001/v1/kv/a | base64 -d
```

To test the raw TCP endpoint we'll use the following files:

```
$ xxd packet.get.dat
00000000: 0000 0000 056d 796b 6579                 .....mykey
$ xxd packet.put.dat
00000000: 0100 0000 056d 796b 6579 6d79 7661 6c    .....mykeymyval
```

The files above are example get/put commands of our trivial binary protocol.
We can create them easily in IEx with `:file.write_file/2`:

```
:file.write_file("packet.get.dat", <<0 :: size(8), 5 :: size(32), "mykey">>)
```

See `lib/mkv/protocol.ex` for the encoding and decoding rules.
Testing:

```
# store mykey:myval
cat packet.put.dat | nc localhost 4002
# fetch mykey via REST
curl http://localhost:4001/v1/kv/mykey | base64 -d
# fetch mykey via raw TCP
cat packet.get.dat | nc -i 1 localhost 4002
```

Let's also test the other direction, i.e. REST -> TCP:

```
curl -X PUT -d $(echo asd | base64) http://localhost:4001/v1/kv/mykey
cat packet.get.dat | nc -i 1 localhost 4002
```

To test the UDP endpoint, we'll use the same binary protocol files as with raw TCP.

Caveat: in my tests the MacOS X Catalina netcat (aka `nc`) could not properly send a UDP datagram
to an Elixir UDP server. As of that, whilst with TCP we use netcat, for UDP we use `socat`.

```
# store mykey:myval via the UDP endpoint
cat packet.put.dat | socat -d - UDP:localhost:4003
# retrieve via REST
curl http://localhost:4001/v1/kv/mykey | base64 -d
# retrieve via UDP
cat packet.get.dat | socat -d - UDP:localhost:4003
```
