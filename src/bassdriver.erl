-module(bassdriver).

-define(DRV_INIT, $I).
-define(DRV_PLAY, $P).
-define(DRV_STOP, $S).
-define(DRV_SET_POS, $T).

-export([init/1, play/2, stop_playing/1, start/1, stop/1, set_position/2]).

start(Libname) ->
    case erl_ddll:load_driver("../priv/", "bassdriver") of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Message} -> exit(erl_ddll:format_error(Message))
    end,
    register_lib("bassdriver", Libname).

init(Libname) ->
    io:format("Initializing..."),
    control(Libname, ?DRV_INIT, "").

play(Libname, File) ->
    io:format("Playing..."),
    control(Libname, ?DRV_PLAY, File).

stop_playing(Libname) ->
    io:format("Stopping Playback..."),
    control(Libname, ?DRV_STOP, ""),
    case erl_ddll:unload_driver("bassdriver") of
        ok -> ok;
        {error, Message} -> exit(erl_ddll:format_error(Message))
    end.

set_position(Libname, Pos) ->
    control(Libname, ?DRV_SET_POS, Pos).

stop(Libname) ->
    stop_playing(Libname),
    [{port, Port}| _] = ets:lookup(Libname, port),
    Port ! {close, self()},
    ok.

register_lib(SharedLib, Libname) ->
    Port = open_port({spawn, SharedLib}, []),
    Tab = ets:new(Libname, [bag, named_table]),
    ets:insert(Tab, {port, Port}).

control(Libname, Cmd, Data) ->
    [{port, Port}| _] = ets:lookup(Libname, port),
    erlang:port_control(Port, Cmd, Data).