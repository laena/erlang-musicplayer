-module(trackplayer).
-export([play_track/2, loop/2, init/2]).

init(Name, StartTime) ->
    % CurrentDirName = filename:basename(filename:absname("")),
    % io:format("cwd: ~p\n", [CurrentDirName]),
    % io:format("Hello from Trackplayer ~p\n",[Name]),
    State = spawn_link(trackplayer, loop, [Name, StartTime]),
    {ok, State}.


play_track(Name, StartTime) ->
    os:cmd(io_lib:format("cvlc ~p --start-time ~p --play-and-exit", [Name, StartTime])),
    exit(stopped).

loop(Name, StartTime) ->
    receive
        {sync_position, Pos} ->
            play_track(Name, Pos),
            loop(Name, StartTime)
    end.