-module(orchestra).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("../include/config.hrl").

start_link() ->
    supervisor:start_link({local, orchestra}, ?MODULE, []).

init([]) ->
    io:format("Init from Orchestra\n"),
    RestartStrategy = {one_for_one, 20, 1}, % restart at most 10/second
    ChildSpec1 = {c1, {trackplayer, init, [?FILENAME1, ?STARTPOS]},
            permanent, brutal_kill, worker, [trackplayer]},
    ChildSpec2 = {c2, {trackplayer, init, [?FILENAME2, ?STARTPOS]},
            permanent, brutal_kill, worker, [trackplayer]},
    ChildSpec3 = {c3, {trackplayer, init, [?FILENAME3, ?STARTPOS]},
            permanent, brutal_kill, worker, [trackplayer]},
    ChildSpec4 = {c4, {trackplayer, init, [?FILENAME4, ?STARTPOS]},
            permanent, brutal_kill, worker, [trackplayer]},

    MonitorSpec = {m, {orchestramonitor, start_link, [?STARTPOS]}, permanent, brutal_kill, worker, [orchestramonitor]},

    {ok, {RestartStrategy, [ChildSpec1, ChildSpec2, ChildSpec3, ChildSpec4, MonitorSpec]}}.