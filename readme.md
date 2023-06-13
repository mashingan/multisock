# Multisock

A pragma that works like [`async.multisync`](https://nim-lang.org/docs/asyncmacro.html#multisync.m,untyped)
(which only strip off `await`) with additional functionalities like:

+ Adjust returning future signature to simply returning object.
+ Strip `asyncCheck`.
+ Adjust `waitfor` in `asyncfuture.async` to be `await` async operation and `waitFor` for sync operation.
+ Support not only function but object too.

Initially private module only used for supporting Anonimongo which then useful for other uses too
hence it's created as a separate module by itself.

# Used in

1. [Anonimongo](https://github.com/mashingan/anonimongo).

# License

MIT