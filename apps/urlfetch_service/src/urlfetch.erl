-module(urlfetch).

-export([start/0]).

start() ->
    application:start(urlfetch_service).
