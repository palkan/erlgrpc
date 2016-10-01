[![Build Status](https://travis-ci.org/palkan/erlgrpc.svg?branch=master)](https://travis-ci.org/palkan/erlgrpc)

Erlang GRPC
=====

It's a the very-very-very first attempt to create Erlang [grpc](grpc.io) client.

See [specs](tree/master/specs) for working examples.

## How to run specs

We use Ruby GRPC server for testing our client (it is launched automatically when running Common Test specs).
Required Ruby version: >= 2.2.

```shell
# run specs
rebar3 ct
```