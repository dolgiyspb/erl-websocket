-module(erlang_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_socket_server/1]).

%% Supervisor callbacks
-export([init/1]).
-include ("socket_server.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->	
    %supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
    supervisor:start_link(?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
	io:format("Init application~n"),
	%{ok, Port} = application:get_env(port),
	%{ok, Socket} = gen_tcp:listen(Port, [ binary, {active, once} ]),
	Pid = self(),
	spawn_link( fun () -> spawn_listeners(Pid) end ),
    {ok, { {simple_one_for_one, 5, 10}, [ ?CHILD(socket_server, worker, [Args#state{ pid = Pid }]) ]} }.


spawn_listeners(Pid) ->
	[start_socket_server(Pid) || _ <- lists:seq(1,20)],
	ok.

start_socket_server(Pid) ->	
	{ok, _Child} = supervisor:start_child(Pid, []).

