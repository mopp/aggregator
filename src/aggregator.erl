%%%-------------------------------------------------------------------
%% @doc aggregator public API.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator).

%% API
-export([start/0,
         put/3,
         fetch/2,
         count/2,
         stop/1]).

-export_type([key/0,
              value/0]).

-type key() :: term().
-type value() :: term().
-type counts() :: #{value() => non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, pid()}.
start() ->
    aggregator_sup:start_child().

%% @doc put new data.
%% The put data associated with the caller.
%% It will be removed when the caller is exit.
-spec put(pid(), key(), value()) -> ok.
put(Pid, Key, Value) ->
    gen_server:cast(Pid, {put, self(), Key, Value}).

-spec fetch(pid(), key()) -> {ok, value()} | {error, no_key}.
fetch(Pid, Key) ->
    gen_server:call(Pid, {fetch, self(), Key}).

-spec count(pid(), key()) -> {ok, counts()} | {error, no_key}.
count(Pid, Key) ->
    gen_server:call(Pid, {count, Key}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    aggregator_sup:stop_child(Pid).
