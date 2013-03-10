-module(urlfetch_handler).

-behaviour(gen_fsm).

-include("urlfetch.hrl").

-export([start_link/0, set_socket/2]).

%% Finite state machine API
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

-export(['AWAIT_SOCKET'/2, 'AWAIT_DATA'/2]).

-record(state, {socket, addr, id}).


start_link() ->
    gen_fsm:start_link(?MODULE, [], []).


set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).


init([]) ->
    process_flag(trap_exit, true),
    {ok, 'AWAIT_SOCKET', #state{}}.


%% Client connects
'AWAIT_SOCKET'({socket_ready, Socket}, State) when is_port(Socket) ->
    inet:setopts(Socket, [binary, {active, once}]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    error_logger:info_msg("~p Client ~p connected.~n", [self(), IP]),
    {next_state, 'AWAIT_DATA', State#state{socket=Socket, addr=IP}, ?TIMEOUT};
'AWAIT_SOCKET'(Other, State) ->
    error_logger:error_msg("Unexpected message: ~p~n", [Other]),
    {next_state, 'AWAIT_SOCKET', State}.


%% Notification event coming from client
'AWAIT_DATA'({data, Data}, #state{socket=S} = State) ->
    case urlfetch_rpc_parser:parse(Data) of
        {request, <<"FETCH_ASYNC">>, MethodString, Url, Payload, Headers} ->
            Id = urlfetch_uuid:new(),
            error_logger:info_msg("~p Fetching ~p.~n", [self(), Url]),
            Method = get_method(MethodString),
            Result = urlfetch_async:fetch({
                Id, Method, binary_to_list(Url), binary_to_list(Payload),
                urlfetch_http:decode_headers(Headers)}),
            case Result of
                ok ->
                    gen_tcp:send(S, Id);
                error ->
                    gen_tcp:send(S, <<"ERROR">>)
            end,
            NewState = State#state{id=Id},
            {next_state, 'AWAIT_DATA', NewState, ?TIMEOUT};
        {request, <<"GET_RESULT">>, Id} ->
            case get_result(Id) of
                {result, Result} ->
                    gen_tcp:send(S, [pack_result(Result), "\tEOF\n"]),
                    spawn(urlfetch_async, purge, [Id]);
                {error, not_found} ->
                    gen_tcp:send(S, pack_result({404, "NOT_FOUND"}))
            end,
            {next_state, 'AWAIT_DATA', State, ?TIMEOUT};
        {request, <<"GET_RESULT_NOWAIT">>, Id} ->
            case get_result({nowait, Id}) of
                {result, Result} ->
                    gen_tcp:send(S, [pack_result(Result), "\tEOF\n"]),
                    spawn(urlfetch_async, purge, [Id]);
                {error, not_found} ->
                    gen_tcp:send(S, pack_result({404, "NOT_FOUND"}))
            end,
            {next_state, 'AWAIT_DATA', State, ?TIMEOUT};
        {request, _, _} ->
            gen_tcp:send(S, <<"ERROR">>),
            {next_state, 'AWAIT_DATA', State, ?TIMEOUT};
        {noreply, _} ->
            gen_tcp:send(S, <<"ERROR">>),
            {stop, normal, State}
    end;
'AWAIT_DATA'(timeout, State) ->
    error_logger:error_msg("~p Closing connection (timeout).~n", [self()]),
    {stop, normal, State};
'AWAIT_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p~n", [self(), Data]),
    {next_state, 'AWAIT_DATA', State, ?TIMEOUT}.


handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.


handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.


handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);
handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket} = StateData) ->
    {stop, normal, StateData}.


terminate(_Reason, _StateName, #state{socket=Socket, addr=Addr}) ->
    (catch gen_tcp:close(Socket)),
    error_logger:info_msg("~p Client ~p disconnected.~n", [self(), Addr]),
    ok.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% Internal API

get_method(String) ->
    {ok, [{atom, 1, Method}], 1} = erl_scan:string(binary_to_list(String)),
    Method.


get_result({nowait, Id}) ->
    case urlfetch_async:get_result(Id) of
        {result, Result} ->
            {result, Result};
        {error, _} ->
            {error, not_found}
    end;
get_result(Id) ->
    case urlfetch_async:get_result(Id) of
        {result, Result} ->
            {result, Result};
        {error, retry} ->
            timer:sleep(50),
            get_result(Id);
        {error, not_found} ->
            {error, not_found}
    end.


pack_result(Result) ->
    {Code, Body} = Result,
    case is_binary(Body) of
        true ->
            Data = Body;
        false ->
            Data = list_to_binary(Body)
    end,
    <<Code:32, Data/binary>>.
