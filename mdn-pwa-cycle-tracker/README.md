# Progressive Web App

This project follows the [MDN CycleTracker PWA tutorial][mdn-ct-tutorial].

[mdn-ct-tutorial]: https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps/Tutorials/CycleTracker

To install a PWA it has to be served over a secure connection, so HTTPS.
One way to do it is forwarding with [ngrok](https://ngrok.com/).
Just use a Python3 simple HTTP server locally:

```sh
python3 -m http.server 9000
```

And then set up forwarding with ngrok:

```sh
ngrok http http://localhost:9000
```

The ngrok provided endpoint is served over HTTPS,
so if all the other conditions are met, the PWA can be installed over this connection.
