# Factorial service

Create a service for calculating the factorial - you can use the solution from `ex2`.
The service should block each client until a response is available,
but at the same time be able to handle requests concurrently.
In other words, if client A sends a request and then B sends a request,
but the result for B's request is available first,
then B should get that result and unblock first.
