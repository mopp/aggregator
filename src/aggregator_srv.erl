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

-record(?STATE, {data_map = #{} :: data()}).

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

handle_call({fetch, Key}, _, State = #?STATE{data_map = DataMap}) ->
    Result = case DataMap of
                 #{Key := Values} ->
                     {ok, Values};
                 _ ->
                     {error, no_key}
             end,
    {reply, Result, State}.

handle_cast({append, Key, Value}, State = #?STATE{data_map = DataMap0}) ->
    DataMap = case DataMap0 of
                  #{Key := Values} ->
                      DataMap0#{Key => [Value | Values]};
                  _ ->
                      DataMap0#{Key => [Value]}
              end,
    {noreply, State#?STATE{data_map = DataMap}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
