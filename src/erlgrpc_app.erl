-module(erlgrpc_app).
-author(palkan).
-include_lib("erlgrpc/include/erlgrpc.hrl").
-include_lib("erlgrpc/include/log.hrl").
-include_lib("erlgrpc/include/priv.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ?I("Starting application: erlgrpc"),
  ConfigPath = case ?Config(config, undefined) of
    undefined -> "erlgrpc.config";
    Else -> Else
  end,
  ulitos_app:load_config(?APP, ConfigPath, ["etc"]),
  case ?Config(use_sync, false) of
    true -> sync:go();
    false -> pass
  end,
  erlgrpc_sup:start_link().

stop(_State) ->
  ok.
