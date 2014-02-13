-module (socket_server).
-behaviour (gen_server).

-record (state, { socket }).
-include ("socket_server.hrl").
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
	{ok, ActiveSocket} = gen_tcp:accept(ListenSocket),
	erlang_socket_sup:start_socket_server(),
	{noreply, S#state{ socket=ActiveSocket } }.

handle_info({tcp, Socket, Str}, State) ->	
	io:format("~p~n", [request_parser:parse_request(Str)]),
	Request = request_parser:parse_request(Str),
	accept_websocket_connection(Request, Socket),
	{noreply, State}.

code_change(_,_,_) ->
	ok.
terminate(Reason, State) ->
	gen_tcp:close(State#state.socket),
	{terminate, Reason, State}.

accept_websocket_connection({ { headers, Headers }, _Body}, Socket) ->	
	{ ?WEBSOCKET_KEY_HEADER, WebsocketKey} = header_parser:get_header(?WEBSOCKET_KEY_HEADER, Headers),
	KeyWithGuid = <<WebsocketKey/bits, ?GUID_STRING/bits>>,
	AcceptString = make_accept_websocket_connection_string(binary:list_to_bin(base64:encode_to_string(crypto:sha(KeyWithGuid))), <<"chat">>), 
	send(Socket, AcceptString),
	io:format("~p~n", [KeyWithGuid]).

send(Socket, Message) ->
	ok = gen_tcp:send(Socket, Message),
	ok = inet:setopts(Socket, [{active, once}]),
	ok.

make_accept_websocket_connection_string(SecKey, Proto) ->
	<<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ",  SecKey/bits, "\r\n\r\n">>.
	%"\r\nSec-WebSocket-Protocol: ", Proto/bits ,"\n\n">>.

handle_request(Request) ->
	Request.






