# Kubernetes HorizontalPodAutoscaler

See:

- https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale-walkthrough/
- https://kubernetes.io/docs/tasks/run-application/horizontal-pod-autoscale/
- https://docs.aws.amazon.com/eks/latest/userguide/horizontal-pod-autoscaler.html

For generating load, which is not captured in `hpa.log`, use:

```
kubectl run -i --tty load-generator --rm --image=busybox:1.28 --restart=Never -- /bin/sh -c "while sleep 0.01; do wget -q -O- http://php-apache; done"
```

In general, follow the walkthrough linked above - all the steps to setup
HPA are described there.
