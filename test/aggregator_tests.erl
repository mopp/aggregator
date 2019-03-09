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
     process_tests()}.

process_tests() ->
    [{"Start and stop aggregator",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ?assert(is_process_alive(Pid)),
              ok = aggregator:stop(Pid),
              ?assertNot(is_process_alive(Pid))
      end}].