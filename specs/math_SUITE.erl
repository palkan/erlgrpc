-module(math_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("erlgrpc/include/erlgrpc.hrl").
-include_lib("erlgrpc/include/log.hrl").
-include_lib("erlgrpc/include/priv.hrl").
-include("protos/math_pb.hrl").

-compile(export_all).

init_per_suite(Config) ->
  lager:start(),
  Config.

end_per_suite(_) ->
  application:stop(lager),
  ok.

init_per_group(Group, Config) ->
  erlgrpc:start(),
  {ok, Pid} = erlgrpc:dial(#{}),
  [{server, Pid} | Config].

end_per_group(_, _Config) ->
  erlgrpc:stop(),
  ok.

all() ->
  [
    {group, simple_tests}
  ].

groups() ->
  [
    {
      simple_tests, [shuffle, parallel, {repeat, to_int(os:getenv("N", 5))}],
      [
        simple_test,
        simple_test_2
      ]
    }
  ].

simple_test(Config) ->
  Pid = ?config(server, Config),
  {ok, Res} = math_service:add(Pid, 2, 3),
  5 = Res#'OperationReply'.result.

simple_test_2(Config) ->
  Pid = ?config(server, Config),
  {ok, Res} = math_service:multiply(Pid, 2, 3),
  6 = Res#'OperationReply'.result.

to_int(L) when is_list(L) ->
  list_to_integer(L);

to_int(N) -> N.
