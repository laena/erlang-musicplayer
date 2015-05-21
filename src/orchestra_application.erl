-module(orchestra_application).
-behaviour(application).

-export([start/0, start/2, stop/1, shutdown/0]).

start() ->
    case orchestra:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

start(normal, _Args) ->
    case orchestra:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end;

start({takeover, _OtherNode}, []) ->
    case orchestra:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

stop(_State) ->
    ok.

shutdown() ->
	application:stop(orchestra_application).