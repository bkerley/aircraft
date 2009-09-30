-module(aircraft).
-behaviour(application).

-export([
         start/2,
         stop/1
         ]).

start(_Type, StartArgs) ->
    case air_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
