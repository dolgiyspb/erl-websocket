-module(erlang_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket_server/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->	
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	io:format("Init application~n"),
	{ok, Port} = application:get_env(port),
	{ok, Socket} = gen_tcp:listen(Port, [ binary, {active, once} ]),
	spawn_link(fun spawn_listeners/0),
    {ok, { {simple_one_for_one, 5, 10}, [ ?CHILD(socket_server, worker, [Socket]) ]} }.


spawn_listeners() ->
	[start_socket_server() || _ <- lists:seq(1,20)],
	ok.

start_socket_server() ->	
	{ok, _Child} = supervisor:start_child(?MODULE, []).

