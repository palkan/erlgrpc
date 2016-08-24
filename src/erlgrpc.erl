%% Copyright
-module(erlgrpc).
-author(palkan).
-include_lib("erlgrpc/include/erlgrpc.hrl").
-include_lib("erlgrpc/include/log.hrl").
-include_lib("erlgrpc/include/priv.hrl").
-define(APPS, [lager]).

%% ------------------------------------------------------------------
%% Common Application Function Exports
%% ------------------------------------------------------------------

-export([start/0, dial/1, stop/0, upgrade/0, ping/0, invoke/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-define(SERVER, erlgrpc_server).

start() ->
  ulitos_app:ensure_started(?APPS),
  application:start(erlgrpc).

stop() ->
  application:stop(erlgrpc).

upgrade() ->
  ulitos_app:reload(erlgrpc),
  ok.
 
ping() ->
  pong.

%% @doc
%% Connects to RPC server.
%% Options:
%% - host - RPC server URI
%% @end
-spec dial(Options::map()) -> {ok, Pid::pid()}.
dial(Options) ->
  erlgrpc_sup:start_client(Options).

%% @doc
%% Invoke RPC method and return response or error.
%% @end
-spec invoke(ClientPid::pid(), Method::string(), In::binary()) -> {ok, Out::binary()} | {error, Reason::string()}.
invoke(ClientPid, Method, In) ->
  gen_server:call(ClientPid, {invoke, Method, In}).
