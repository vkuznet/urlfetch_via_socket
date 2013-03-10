%% -*- mode: erlang -*-
-module(urlfetch_service_sup).
-behaviour(supervisor).
-export([start_client/0,start_link/0]). %% Supervisor APIs
-export([init/1]). %% Supervisor callbacks.

-include("urlfetch.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() ->
    supervisor:start_child(urlfetch_client_sup, []).

%% A startup function for service supervisors, will call init([Port, Module])
start_link() ->
    ListenPort = get_app_env(listen_port, ?PORT),
    supervisor:start_link({local, ?MODULE}, ?MODULE,
                          [ListenPort, urlfetch_handler]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP listener
              {   urlfetch_listener_sup,
                  {urlfetch_listener, start_link, [Port, Module]},
                  permanent,
                  2000,
                  worker,
                  [urlfetch_listener]
              },
              % UUID generator
              {   urlfetch_uuid_sup,
                  {urlfetch_uuid, start_link, []},
                  permanent,
                  2000,
                  worker,
                  [urlfetch_uuid]
              },
              % ETS Cache
              {   urlfetch_cache_sup,
                  {urlfetch_cache, start_link, []},
                  permanent,
                  2000,
                  worker,
                  [urlfetch_cache]
              },
              % Client instance supervisor
              {   urlfetch_client_sup,
                  {supervisor, start_link, [{local, urlfetch_client_sup},
                                            ?MODULE, [Module]]},
                  permanent,
                  infinity,
                  supervisor,
                  []
              }
            ]
        }
    };
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % TCP Client
              {   undefined,
                  {Module, start_link, []},
                  temporary,
                  2000,
                  worker,
                  []
              }
            ]
        }
    }.

%% ===================================================================
%% Local helpers
%% ===================================================================

get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
        case init:get_argument(Opt) of
        [[Val | _]] -> Val;
        error       -> Default
        end
    end.
