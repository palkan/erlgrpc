-module(erlgrpc_sup).
-author(palkan).
-include_lib("erlgrpc/include/priv.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0, start_client/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, StartArgs), #{
  id => I,
  start => {I, start_link, StartArgs},
  restart => permanent,
  shutdown => 5000,
  type => Type,
  modules => [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client(Options) ->
  supervisor:start_child(
    ?MODULE,
    ?CHILD(erlgrpc_client, worker, [Options])
  ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, []}}.
