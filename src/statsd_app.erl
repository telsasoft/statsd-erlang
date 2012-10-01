-module(statsd_app).
-author("Martin Donath").
-behaviour(application).

-export([start/2, stop/1]).

%% Public: starts the statsd server
%%
%% returns a #state record containing the socket
start(_Type, Args) ->
  erlang:apply(statsd, start, Args).

%% Public: stops the statsd server
%%
%% returns ok
stop(_State) ->
  ok.