%%%-------------------------------------------------------------------
%% @doc aggregator top level supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(aggregator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         stop/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | {shutdown, term()} | term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    true = exit(Pid, shutdown),
    ok.

%%====================================================================
%% `supervisor' callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok,
     {#{strategy => simple_one_for_one},
      [#{id => aggregator_srv,
         start => {aggregator_srv, start_link, []},
         restart => permanent,
         type => worker,
         modules => [aggregator_srv]}]}}.
