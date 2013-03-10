%% -*- mode: erlang -*-
-module(urlfetch_service_app).
-behaviour(application).
-export([start/2, stop/1]). %% Application callbacks.

start(_Type, _Args) ->
    inets:start(),
    ssl:start(),
    urlfetch_service_sup:start_link().

stop(_S) ->
    ok.
