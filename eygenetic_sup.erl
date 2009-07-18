
-module(eygenetic_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(eygenetic_sup, []).
    
init(_Args) ->
    {ok, {{one_for_one, 10, 60},
          [{eygenetic, {eygenetic, start, []},
            permanent, brutal_kill, worker, [eygenetic]}]}}.