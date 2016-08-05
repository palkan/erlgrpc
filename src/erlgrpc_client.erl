-module(erlgrpc_client).
-author(palkan).
-include_lib("erlgrpc/include/erlgrpc.hrl").
-include_lib("erlgrpc/include/log.hrl").
-include_lib("erlgrpc/include/priv.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(state, {
  client ::pid()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#{ host := Host, port := Port }) ->
  {ok, Pid} = h2_client:start_link(http, Host, Port),
  {ok, #state{client = Pid}};
 
init(#{ host := Host }) ->
  init(#{ host => Host, port => 50051});

init(_) ->
  init(#{ host => "localhost", port => 50051}).

handle_call({invoke, Method, Data}, _From, #state{client = Client} = State) ->
  RequestHeaders = [
    {<<":method">>, <<"POST">>},
    {<<":path">>, Method},
    {<<":scheme">>, <<"http">>},
    {<<":authority">>, <<"localhost">>},
    {<<"grpc-timeout">>, <<"5S">>},
    {<<"content-type">>, <<"application/grpc+proto">>},
    {<<"grpc-encoding">>, <<"identity">>},
    {<<"grpc-accept-encoding">>, <<"identity">>},
    {<<"grpc-message-type">>, <<"/anycable.RPC/ConnectionRequest">>},
    {<<"user-agent">>, <<"chatterbox-client/0.0.1">>}
  ],

  Size = size(Data),
  Compressed = 0,
  RequestBody = <<Compressed:32, Size:32, Data/binary>>,

  ?D({request, RequestHeaders, RequestBody}),

  Res = h2_client:sync_request(Client, RequestHeaders, RequestBody),
  {reply, Res, State};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
