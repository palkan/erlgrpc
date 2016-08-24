-module(math_service).
-include("math_pb.hrl").

-export([add/3, multiply/3]).

add(Client, A, B) ->
  case erlgrpc:invoke(Client, <<"/testmath.Calculator/Add">>, math_pb:encode_msg(#'OperationRequest'{ a = A, b = B})) of
    {ok, Res} -> {ok, math_pb:decode_msg(Res, 'OperationReply')};
    Else -> Else
  end.
  

multiply(Client, A, B) ->
  case erlgrpc:invoke(Client, <<"/testmath.Calculator/Multiply">>, math_pb:encode_msg(#'OperationRequest'{ a = A, b = B})) of
    {ok, Res} -> {ok, math_pb:decode_msg(Res, 'OperationReply')};
    Else -> Else
  end.
