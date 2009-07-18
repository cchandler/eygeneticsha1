-module(eygenetic_app).
-behaviour(application).

-export([start/2, stop/1,go/0]).

start(_Type, _Args) ->
    eygenetic_sup:start_link().

stop(_State) ->
    io:format("Eygenetic server terminating~n"),
    ok.

go() ->
    application:start(eygenetic).