%%%-------------------------------------------------------------------
%% @doc aggregator public API.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator).

%% API
-export([start/0,
         stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, pid()}.
start() ->
    aggregator_sup:start_child().

-spec stop(pid()) -> ok.
stop(Pid) ->
    aggregator_sup:stop_child(Pid).
