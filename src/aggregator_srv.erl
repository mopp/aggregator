%%%-------------------------------------------------------------------
%% @doc aggregator server.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type key() :: aggregator:key().
-type value() :: aggregator:value().
-type counts() :: aggregator:counts().
-type process_map() :: #{pid() => #{key() => value()}}.

-define(STATE, ?MODULE).

-record(?STATE, {process_map = #{} :: process_map()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% `gen_server' callbacks
%%%===================================================================

init([]) ->
    {ok, #?STATE{}}.

handle_call({fetch, Pid, Key}, _, State = #?STATE{process_map = ProcessMap}) ->
    {reply, handle_fetch(Pid, Key, ProcessMap), State};
handle_call({count, Key}, _, State = #?STATE{process_map = ProcessMap}) ->
    {reply, handle_count(Key, ProcessMap), State}.

handle_cast({put, Pid, Key, Value}, State = #?STATE{process_map = ProcessMap}) ->
    {noreply, State#?STATE{process_map = handle_put(Pid, Key, Value, ProcessMap)}}.

handle_info({'DOWN', _, process, Pid, _}, State = #?STATE{process_map = ProcessMap}) ->
    {noreply, State#?STATE{process_map = maps:remove(Pid, ProcessMap)}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_put(pid(), key(), value(), process_map()) -> process_map().
handle_put(Pid, Key, Value, ProcessMap0) ->
    _ = monitor(process, Pid),

    case ProcessMap0 of
        #{Pid := Map} ->
            %% This process has some data already.
            ProcessMap0#{Pid => maps:put(Key, Value, Map)};
        _ ->
            %% put the given data.
            ProcessMap0#{Pid => #{Key => Value}}
    end.

-spec handle_fetch(pid(), key(), process_map()) -> {ok, value()} | {error, no_key}.
handle_fetch(Pid, Key, ProcessMap) ->
    case ProcessMap of
        #{Pid := #{Key := Value}} ->
            {ok, Value};
        _ ->
            {error, no_key}
    end.

-spec handle_count(pid(), process_map()) -> {ok, counts()} | {error, no_key}.
handle_count(Key, ProcessMap) ->
    Counts = count_values(Key, ProcessMap),
    case maps:size(Counts) of
        0 ->
            {error, no_key};
        _ ->
            {ok, Counts}
    end.

-spec count_values(pid(), process_map()) -> counts().
count_values(Key, ProcessMap) ->
    lists:foldl(fun(#{Key := Value}, Acc) ->
                        maps:put(Value, maps:get(Value, Acc, 0) + 1, Acc);
                   (_, Acc) ->
                        Acc
                end,
                #{},
                maps:values(ProcessMap)).
