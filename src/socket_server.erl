-module (socket_server).
-behaviour (gen_server).

-record (state, { socket }).
-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2, code_change/3, handle_call/3, get_state/0, terminate/2]).

start_link(Socket) ->
	io:format("Start socket_server~n"),
	Result = gen_server:start_link( ?MODULE, Socket, []),
	io:format("Start socket_server, result ~p~n", [Result]),
	Result.

init(Socket) ->
	io:format("Init socket_server~n"),
	gen_server:cast(self(), accept),
	{ ok, #state{socket = Socket} }.

get_state() ->
	gen_server:call(?MODULE, get_state).

handle_call(get_state, _Pid, State) ->
	{reply, State, State}.

handle_cast(accept, S = #state{ socket = ListenSocket }) ->
	io:format("Accept message ~n"),
	{ok, ActiveSocket} = gen_tcp:accept(ListenSocket),
	erlang_socket_sup:start_socket_server(),
	{noreply, S#state{ socket=ActiveSocket } }.

handle_info({tcp, _Socket, Str}, State) ->
	io:format(binary:split(Str, <<"\r\n">>, [global])),
	{noreply, State}.

code_change(_,_,_) ->
	ok.
terminate(Reason, State) ->
	{terminate, Reason, State}.





