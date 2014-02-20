-module(erlang_socket_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include ("socket_server.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("Start application ~n"),
	application:start(crypto),
	{ok, Port} = application:get_env(port),
	{ok, Socket} = gen_tcp:listen(Port, [ binary, {active, once} ]),
    {ok, Pid} = erlang_socket_sup:start_link( #state{ socket=Socket } ),
    {ok, Pid, #application_state{socket = Socket}}.

stop(State) ->
    io:format("~p~n", [State]),
    gen_tcp:close(State#application_state.socket).
