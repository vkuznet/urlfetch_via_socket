-module(urlfetch_listener).

-behaviour(gen_server).

%% External API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {listener, acceptor, module}).


start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Module], []).


init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {reuseaddr, true}, {backlog, 30}, {active, false}],
    case gen_tcp:listen(Port, Opts) of
    {ok, LSocket} ->
        %% Create first accepting process.
        error_logger:info_msg("~p Listening on ~p.~n", [self(), Port]),
        {ok, Ref} = prim_inet:async_accept(LSocket, -1),
        {ok, #state{listener=LSocket, acceptor=Ref,  module=Module}};
    {error, Msg} ->
        {stop, Msg}
    end.

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSocket, Ref, {ok, CSocket}},
            #state{listener=LSocket, acceptor=Ref, module=Module} = State) ->
    try
        case set_sockopt(LSocket, CSocket) of
        ok              -> ok;
        {error, Reason} -> exit({set_sockopt, Reason})
        end,

        %% New client connected.  Spawn a new process using a supervisor.
        {ok, Pid} = urlfetch_service_sup:start_client(),
        gen_tcp:controlling_process(CSocket, Pid),

        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CSocket),

        %% Signal the network driver that we are ready to accept another
        %% connection.
        case prim_inet:async_accept(LSocket, -1) of
        {ok,    NewRef} -> ok;
        {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,

        {noreply, State#state{acceptor=NewRef}}
    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.~n", [Why]),
        {stop, Why, State}
    end;
handle_info({inet_async, LSocket, Ref, Error}, #state{listener=LSocket, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.~n", [Error]),
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(LSocket, CSocket) ->
    true = inet_db:register_socket(CSocket, inet_tcp),
%    OptionKeys = [active, nodelay, keepalive, delay_send, priority, tos],
%    OptionKeys = [active, nodelay, keepalive],
    OptionKeys = [],
    case prim_inet:getopts(LSocket, OptionKeys) of
        {ok, Opts} ->
            case prim_inet:setopts(CSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(CSocket), Error
            end;
        Error ->
            gen_tcp:close(CSocket), Error
    end.
