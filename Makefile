all: protos

protos: protos-go protos-ruby protos-erl

protos-go:
	protoc --proto_path=./files/protos --go_out=plugins=grpc:./files/gorpc/protos ./files/protos/rpc.proto

protos-ruby:
	protoc --ruby_out=./files/rubyrpc/ --grpc_out=./files/rubyrpc/ --proto_path=./files/protos --plugin=protoc-gen-grpc=`which grpc_tools_ruby_protoc_plugin.rb` ./files/protos/rpc.proto

protos-erl:
	./_build/default/plugins/gpb/bin/protoc-erl -I./files/protos -o-erl ./specs/protos -o-hrl ./specs/protos math.proto

deps-go:
	go get golang.org/x/net/context
	go get google.golang.org/grpc
