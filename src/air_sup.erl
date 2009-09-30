-module(air_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

init(_StartArgs) ->
    AircraftServer = {air_server, {air_server, start_link, 6667},
                      permanent, 2000, worker, [air_server]},
    {ok, {{one_for_one, 0, 1}, [AircraftServer]}}.
