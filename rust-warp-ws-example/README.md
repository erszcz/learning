# Rust WebSocket server with Warp

This is based on the [great LogRocket tutorial](https://blog.logrocket.com/build-websocket-server-with-rust/)!

Register a client:

```sh
$ curl -X POST 'http://localhost:8000/register' -H 'Content-Type: application/json' -d '{ "user_id": 1 }'
{"url":"ws://127.0.0.1:8000/ws/e3f1e380311e435686329fce350487b1"}
```

In another shell, connect to the WebSocket and listen for messages:

```sh
$ websocat -t ws://127.0.0.1:8000/ws/e3f1e380311e435686329fce350487b1
... this doesn't terminate, i.e. it keeps running until Ctrl-C is pressed
```

Publish a message:

```sh
$ curl -X POST 'http://localhost:8000/publish' \
    -H 'Content-Type: application/json' \
    -d '{"user_id": 1, "topic": "cats", "message": "are awesome"}'
```

The last command should result in this message being shown in the shell
where `websocat` is listening:

```sh
are awesome
```
