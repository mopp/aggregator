-module(aggregator_tests).

-include_lib("eunit/include/eunit.hrl").

-define(waitMessage(Term),
        receive
            Term ->
                ok
        after
            1000 ->
                ?assert(false)
        end).

aggregator_test_() ->
    {foreach,
     fun() ->
             {ok, Apps} = application:ensure_all_started(aggregator),
             Apps
     end,
     fun(Apps) ->
             [application:stop(App) || App <- Apps]
     end,
     process_tests() ++
     usage_tests()}.

process_tests() ->
    [{"Start and stop aggregator",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ?assert(is_process_alive(Pid)),
              ok = aggregator:stop(Pid),
              ?assertNot(is_process_alive(Pid))
      end}].

usage_tests() ->
    [{"Aggregate numbers",
      fun() ->
              {ok, Pid} = aggregator:start(),
              aggregator:append(Pid, numbers, 1),
              aggregator:append(Pid, numbers, 2),
              aggregator:append(Pid, numbers, 3),

              ?assertEqual({ok, [3, 2, 1]}, aggregator:fetch(Pid, numbers)),
              aggregator:stop(Pid)
      end},
     {"Aggregator returnes {error, no_key} if it not have the given key",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ?assertEqual({error, no_key}, aggregator:fetch(Pid, eunit)),
              aggregator:stop(Pid)
      end},
     {"Aggregator deletes value if associated process is down",
      fun() ->
              {ok, Pid} = aggregator:start(),
              Self = self(),
              Worker = spawn(fun() ->
                                     aggregator:append_monitor(Pid, numbers, 1),
                                     Self ! done,
                                     ?waitMessage(break)
                             end),
              monitor(process, Worker),
              ?waitMessage(done),

              ?assertEqual({ok, [1]}, aggregator:fetch(Pid, numbers)),

              Worker ! break,
              ?waitMessage({'DOWN', _, process, Worker, _}),

              ?assertNot(is_process_alive(Worker)),
              ?assertEqual({error, no_key}, aggregator:fetch(Pid, numbers)),

              aggregator:stop(Pid)
      end}].
