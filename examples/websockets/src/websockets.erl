-module(websockets).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(websockets).
