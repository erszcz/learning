# Test conclusions

While in a happy case the `Fun` passed to `global:trans` is executed only once,
in a situation when this function itself crashes,
the transaction lock is deleted.
Therefore, if at this point some node yet has to attempt taking the lock,
it will (might?) succeed and try executing the same `Fun` in its own context.

This is not bad per se, but care must be taken to ensure that `Fun` is idempotent.
Another result might be duplication of work performed in the cluster.
However, in case of the intended use case - cluster-global cleanup after a
`nodedown` event - it's amount of duplication is still less than
delegating the cleanup operation to all nodes which are still alive.
