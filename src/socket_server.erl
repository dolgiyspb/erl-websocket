-module (socket_server).
-behaviour (gen_server).


-include ("socket_server.hrl").
-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2, code_change/3, handle_call/3, get_state/0, terminate/2]).

start_link(Args) ->
	io:format("Start socket_server~n"),
	Result = gen_server:start_link( ?MODULE, Args, []),
	io:format("Start socket_server, result ~p~n", [Result]),
	Result.

init(Args) ->
	io:format("Init socket_server~n"),
	gen_server:cast(self(), accept),
	{ ok, Args }.
send_message(<<Message/bits>>) ->
	gen_server:cast(self(), { send_message, Message }).

get_state() ->
	gen_server:call(?MODULE, get_state).

handle_call(get_state, _Pid, State) ->
	{reply, State, State}.

handle_cast( { send_message, <<Message/bits>> }, S = #state{ socket = Socket } ) ->
	Length = byte_size(Message),
	Header = <<1:1, 0:1, 0:1, 0:1, 1:4, 0:1, Length:7/unsigned-integer>>,
	HeaderLength = bit_size(Header),
	AlignBits = ( 8 - (HeaderLength rem 8) ),
	M =  list_to_binary([Header, Message, <<0:AlignBits>>]),	
	send(Socket, M  ),
	{ noreply, S };

handle_cast(accept, S = #state{ socket = ListenSocket, pid = Pid }) ->	
	{ok, ActiveSocket} = gen_tcp:accept(ListenSocket),
	erlang_socket_sup:start_socket_server(Pid),
	{noreply, S#state{ socket=ActiveSocket } }.

handle_info({tcp, Socket, Str}, State) ->	
	io:format("~p~n", [request_parser:parse_request(Str)]),
	Request = request_parser:parse_request(Str),	
	case Request of 
		{ { headers, _ }, { body, _ } } -> 
			accept_websocket_connection(Request, Socket);
		_ ->
			handle_request(Request, Socket)
	end,
	{noreply, State}.

handle_request(Request, Socket) ->
	io:format("In handle_request ~p~n", [Request]),
	send_message(<<"Test...Test...">>).

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
	io:format("In send ~p Size ~p bits~n", [Message, bit_size(Message) ]),
	ok = gen_tcp:send(Socket, Message),
	ok = inet:setopts(Socket, [{active, once}]),
	ok.

make_accept_websocket_connection_string(SecKey, Proto) ->
	<<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ",  SecKey/bits, "\r\n\r\n">>.	

handle_request(Request) ->
	Request.






