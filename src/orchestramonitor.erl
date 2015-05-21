-module(orchestramonitor).
-behaviour(gen_server).

-export([start_link/1, sync_children/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(INTERVAL, 10).
-define(TRACK_LENGTH, 27000).

start_link(InitialPos) ->
    io:format("Hello from Monitor\n"),
    gen_server:start_link(?MODULE, [InitialPos], []).

init([InitialPos]) ->
    io:format("Init from Monitor\n"),
    State = erlang:send_after(?INTERVAL, self(), {trigger, InitialPos}),
    {ok, State}.

handle_call(Request, _From, State) ->
    io:format("Request to Monitor: ~p\n", [Request]),
    {noreply, State}.

handle_cast(Msg, State) ->
    io:format("Info sent to Monitor: ~p\n", [Msg]),   
    {noreply, State}.

handle_info(Msg, State) ->
    case Msg of
        {trigger, Pos} ->
            Children = supervisor:which_children(orchestra),
            sync_children(Children, Pos),
            NewPos = (Pos + ?INTERVAL) rem ?TRACK_LENGTH,
            erlang:send_after(?INTERVAL, self(), {trigger, NewPos}),
            {noreply, State}
    end.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

sync_children(Children, Pos) ->
    Siblings = [Pid || {_, Pid, _, _} <- Children],
    lists:foreach(
        fun (Child) ->
            case Child == self() of
            true    -> ok;
            false   -> Child ! {sync_position, Pos/1000}
            end
        end, Siblings),
    ok.