-module(aggregator_tests).

-include_lib("eunit/include/eunit.hrl").

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
     notify_tests()}.

process_tests() ->
    [{"Start and stop aggregator",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ?assert(is_process_alive(Pid)),
              ok = aggregator:stop(Pid),
              ?assertNot(is_process_alive(Pid))
      end}].

notify_tests() ->
    [{"Aggregate numbers",
      fun() ->
              {ok, Pid} = aggregator:start(),
              aggregator:append(Pid, numbers, 1),
              aggregator:append(Pid, numbers, 2),
              aggregator:append(Pid, numbers, 3),

              ?assertEqual({ok, [3, 2, 1]}, aggregator:fetch(Pid, numbers))
      end}].
