%%%-------------------------------------------------------------------
%% @doc aggregator public API.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator).

%% API
-export([start/0,
         append/3,
         fetch/2,
         stop/1]).

-export_type([key/0,
              value/0]).

-type key() :: term().
-type value() :: term().

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, pid()}.
start() ->
    aggregator_sup:start_child().

-spec append(pid(), key(), value()) -> ok.
append(Pid, Key, Value) ->
    gen_server:cast(Pid, {append, Key, Value}).

-spec fetch(pid(), key()) -> {ok, [value()]} | {error, no_key}.
fetch(Pid, Key) ->
    gen_server:call(Pid, {fetch, Key}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    aggregator_sup:stop_child(Pid).
