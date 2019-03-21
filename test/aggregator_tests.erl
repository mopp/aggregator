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
     [process_tests(),
      usage_tests()]}.

process_tests() ->
    [{"Start and stop aggregator",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ?assert(is_process_alive(Pid)),
              ok = aggregator:stop(Pid),
              ?assertNot(is_process_alive(Pid))
      end}].

usage_tests() ->
    [
     {"Aggregator stores values",
      fun() ->
              {ok, Pid} = aggregator:start(),
              aggregator:put(Pid, object_id, 100),
              aggregator:put(Pid, some_atoms, [abc, def]),

              ?assertEqual({ok, 100}, aggregator:fetch(Pid, object_id)),
              ?assertEqual({ok, [abc, def]}, aggregator:fetch(Pid, some_atoms)),

              aggregator:stop(Pid)
      end},
     {"aggregator:fetch/2 returnes {error, no_key} if the given key is not found",
      fun() ->
              {ok, Pid} = aggregator:start(),

              ?assertEqual({error, no_key}, aggregator:fetch(Pid, missing_key)),

              aggregator:stop(Pid)
      end},
     {"Aggregator count how many of each value exists, simplest case",
      fun() ->
              {ok, Pid} = aggregator:start(),
              aggregator:put(Pid, object_id, 100),

              ?assertEqual({ok, #{100 => 1}}, aggregator:count(Pid, object_id)),

              aggregator:stop(Pid)
      end},
     {"Aggregator count how many of each value exists, multiple process",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ok = meck:new(aggregator_srv, [passthrough]),

              _ = put_via_worker(Pid, object_id, 100),
              _ = put_via_worker(Pid, object_id, 100),
              _ = put_via_worker(Pid, object_id, 200),

              ok = meck:wait(3, aggregator_srv, handle_cast, '_', 500),

              ?assertEqual({ok, #{100 => 2,
                                  200 => 1}}, aggregator:count(Pid, object_id)),

              _ = meck:unload(aggregator_srv),

              aggregator:stop(Pid)
      end},
     {"aggregator:count/2 returnes {error, no_key} if the given key is not found",
      fun() ->
              {ok, Pid} = aggregator:start(),

              ?assertEqual({error, no_key}, aggregator:count(Pid, missing_key)),

              aggregator:stop(Pid)
      end},
     {"Aggregator deletes value if associated process is down",
      fun() ->
              {ok, Pid} = aggregator:start(),
              ok = meck:new(aggregator_srv, [passthrough]),

              Worker1 = put_via_worker(Pid, object_id, 100),
              _ = put_via_worker(Pid, object_id, 100),
              unlink(Worker1),

              ok = meck:wait(1, aggregator_srv, handle_cast, '_', 500),

              ?assertEqual({ok, #{100 => 2}}, aggregator:count(Pid, object_id)),

              exit_worker(Worker1),
              ok = meck:wait(1, aggregator_srv, handle_info, '_', 500),

              ?assertEqual({ok, #{100 => 1}}, aggregator:count(Pid, object_id)),

              _ = meck:unload(aggregator_srv),

              aggregator:stop(Pid)
      end}
    ].

%% TODO: monitorする前にinsertするプロセスが死亡したとき


put_via_worker(Pid, Key, Value) ->
    spawn_link(fun() ->
                       aggregator:put(Pid, Key, Value),
                       ?waitMessage(finish)
               end).

exit_worker(Pid) ->
    Pid ! finish.
