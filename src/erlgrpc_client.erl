-module(erlgrpc_client).
-author(palkan).
-include_lib("erlgrpc/include/erlgrpc.hrl").
-include_lib("erlgrpc/include/log.hrl").
-include_lib("erlgrpc/include/priv.hrl").
-define(SERVER, ?MODULE).

%% Each response contain Compressed flag (1 byte) and Size info (4 bytes)
-define(RESPONSE_HEADER_SIZE, 40).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  client ::pid(),
  calls = #{} ::map()
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

handle_call({invoke, Method, Data}, From, #state{client = Client, calls = Calls} = State) ->
  RequestHeaders = [
    {<<":method">>, <<"POST">>},
    {<<":scheme">>, <<"http">>},
    {<<":path">>, Method},
    {<<":authority">>, <<"localhost">>},
    {<<"content-type">>, <<"application/grpc">>},
    {<<"user-agent">>, <<"chatterbox-client/0.0.1">>},
    {<<"te">>, <<"trailers">>}
  ],

  Size = size(Data),
  Compressed = 0,
  RequestBody = <<Compressed:8, Size:32, Data/binary>>,

  ?D({request, RequestHeaders, RequestBody}),

  {ok, StreamId} = h2_client:send_request(Client, RequestHeaders, RequestBody),
  {noreply, State#state{calls=Calls#{ StreamId => From }}};

handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'END_STREAM', StreamId}, #state{calls = Calls, client = Client} = State) ->
  case maps:get(StreamId, Calls, undefined) of
    undefined -> ?E({unknown_stream, StreamId});
    From ->
      {ok, {ResponseHeaders, ResponseBody}} = h2_client:get_response(Client, StreamId),
      Status = proplists:get_value(<<"grpc-status">>, ResponseHeaders),
      gen_server:reply(From, parse_response(Status, ResponseBody, ResponseHeaders))
  end,
  {noreply, State#state{calls=maps:remove(StreamId, Calls)}};

handle_info(Info, State) ->
  ?D({unknown, Info}),
  {noreply, State}.

terminate(_Reason, #state{client = Pid} = State) ->
  h2_client:stop(Pid),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec parse_response(Status::binary(), Data::list(binary()), Headers::list()) -> {ok, Res::binary() | nodata} | {error, Reason::binary()}.
parse_response(<<"0">>, [], _) -> {ok, nodata};

parse_response(<<"0">>, [<<_:?RESPONSE_HEADER_SIZE, Data/binary>>], _) ->
  {ok, Data};

parse_response(_Status, _, Headers) ->
  {error, proplists:get_value(<<"grpc-message">>, Headers)}.
