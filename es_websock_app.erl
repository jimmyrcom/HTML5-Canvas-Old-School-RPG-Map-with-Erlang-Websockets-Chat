-module(es_websock_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, StartArgs) ->
    es_websock_supervisor:start_link(StartArgs).

stop(_State) ->
    ok.
