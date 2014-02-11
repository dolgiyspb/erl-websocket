-module(erlang_socket_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("Start application ~n"),
    erlang_socket_sup:start_link().

stop(_State) ->
    ok.
