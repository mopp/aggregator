%%%-------------------------------------------------------------------
%% @doc aggregator application.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator_app).

-behaviour(application).

%% `application' callbacks.
-export([start/2,
         stop/1]).

-type state() :: pid().

%%====================================================================
%% `application' callbacks
%%====================================================================

-spec start(StartType :: term(), StartArgs :: term()) -> {ok, pid(), state()}.
start(_, _) ->
    {ok, Pid} = aggregator_sup:start_link(),
    {ok, Pid, Pid}.

-spec stop(state()) -> ok.
stop(Pid) ->
    aggregator_sup:stop(Pid).
