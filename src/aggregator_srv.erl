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
-type data() :: #{key() => [value()]}.

-define(STATE, ?MODULE).

-record(?STATE, {data_map = #{} :: data(),
                 process_map = #{} :: #{pid() => data()}}).

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

handle_call({fetch, Key}, _, State = #?STATE{data_map = DataMap,
                                             process_map = ProcessMap}) ->
    Result = case DataMap of
                 #{Key := Values} ->
                     {ok, Values};
                 _ ->
                     Values = lists:flatmap(fun(#{Key := V}) ->
                                                    V
                                            end,
                                            maps:values(ProcessMap)),
                     case length(Values) of
                         0 ->
                             {error, no_key};
                         _ ->
                             {ok, Values}
                     end
             end,
    {reply, Result, State}.

handle_cast({append, Key, Value}, State = #?STATE{data_map = Map}) ->
    {noreply, State#?STATE{data_map = update(Map, Key, Value)}};
handle_cast({append_monitor, Key, Value, Pid}, State = #?STATE{process_map = Map0}) ->
    Map = case Map0 of
              #{Pid := DataMap} ->
                  Map0#{Pid => update(DataMap, Key, Value)};
              _ ->
                  _ = monitor(process, Pid),
                  Map0#{Pid => #{Key => [Value]}}
          end,
    {noreply, State#?STATE{process_map = Map}}.

handle_info({'DOWN', _, process, Pid, _}, State = #?STATE{process_map = Map0}) ->
    Map = case maps:is_key(Pid, Map0) of
              true ->
                  maps:remove(Pid, Map0);
              false ->
                  Map0
          end,
    {noreply, State#?STATE{process_map = Map}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
update(Map, Key, Value) ->
    case Map of
        #{Key := Values} ->
            Map#{Key => [Value | Values]};
        _ ->
            Map#{Key => [Value]}
    end.
