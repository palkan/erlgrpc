Erlang GRPC
=====

It's a the very-very-very first attempt to create Erlang [grpc](grpc.io) client.

See [specs](tree/master/specs) for working examples.

## How to run specs

We use Ruby GRPC server for testing our client.

```shell
# run ruby server
ruby files/rubyrpc/server.rb

# compile project
rebar3 compile

# run specs
rebar3 ct
```