-module(statsd_tests).
-include_lib("eunit/include/eunit.hrl").

all_statsd_test_() ->
    [
        {"Should start with defaults",  fun start_with_defaults_test/0},
        {"Should start with default port",  fun start_with_host_test/0},
        {"Should start",  fun start_test/0}
   ].

start_with_defaults_test() ->
    {ok, Pid} = statsd:start(),
    true = is_pid(Pid),
    {ok} = statsd:stop().

start_with_host_test() ->
    {ok, Pid} = statsd:start("localhost"),
    true = is_pid(Pid),
    {ok} = statsd:stop().

start_test() ->
    {ok, Pid} = statsd:start("localhost", 65535), %% custom port
    true = is_pid(Pid),
    {ok} = statsd:stop().
