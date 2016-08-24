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
      simple_tests, [sequence], 
      [
        simple_test
      ]
    }
  ].

simple_test(Config) ->
  Pid = ?config(server, Config),
  {ok, Res} = erlgrpc:invoke(Pid, <<"/testmath.Calculator/Add">>, math_pb:encode_msg(#'OperationRequest'{ a = 2, b = 3})),
  ct:print("Res: ~p", [Res]),
  Decoded = math_pb:decode_msg(Res, 'OperationReply'),
  5 = Decoded#'OperationReply'.result.
