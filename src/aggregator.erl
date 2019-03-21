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

%% @doc Start new aggregator process.
-spec start() -> {ok, pid()}.
start() ->
    aggregator_sup:start_child().

%% @doc Put the given value which associated with the caller.
%% It will be removed when the caller is down.
%% This function uses `gen_server:cast/2'.
-spec put(pid(), key(), value()) -> ok.
put(Pid, Key, Value) ->
    gen_server:cast(Pid, {put, self(), Key, Value}).

%% @doc Fetch value by the given key.
%% You can only get the value that the caller putted before.
-spec fetch(pid(), key()) -> {ok, value()} | {error, no_key}.
fetch(Pid, Key) ->
    gen_server:call(Pid, {fetch, self(), Key}).

%% @doc Count how many of each value exists.
%% This function uses `gen_server:call/2'.
-spec count(pid(), key()) -> {ok, counts()} | {error, no_key}.
count(Pid, Key) ->
    gen_server:call(Pid, {count, Key}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    aggregator_sup:stop_child(Pid).
