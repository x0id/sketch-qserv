%%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 ft=erlang et indentexpr=
%%%--------------------------------------------------------------------------
%%% File:    qserv_worker.erl
%%%
%%% @doc     Accepting connection, serving clients
%%% @author  Dmitriy Kargapolov <dmitriy.kargapolov@idt.net>
%%% @since   22 February 2018
%%% @end
%%%-------------------------------------------------------------------------
-module('qserv_worker').
-author('dmitriy.kargapolov@idt.net').

-behaviour(gen_server).

% external exports
-export([start_link/1]).
-ignore_xref([start_link/1]).

% gen_server callbacks
-export([
    init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3
]).

% external API

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-record(state, {
    sock,       % underlying tcp socket
    left = [],  % unterminated line remainder
    queue       % banker's queue
}).

% gen_server callbacks

init(LSock) ->
    gen_server:cast(self(), {accept, LSock}),
    {ok, init_queue(#state{})}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({accept, LSock}, State) ->
    case gen_tcp:accept(LSock) of
    {ok, Sock} ->
        % start next acceptor/worker
        qserv_listener:start_worker(),
        % continue serving input stream
        {noreply, State#state{sock = Sock}};
    {error, Reason} ->
        error_logger:error_msg("accept error: ~p~n", [Reason]),
        {stop, normal}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp_closed, Sock}, #state{sock = Sock}) ->
    gen_tcp:close(Sock),
    supervisor:terminate_child(qserv_listener, self()),
    {noreply, #state{}};

handle_info({tcp, Sock, Data}, State = #state{sock = Sock}) ->
    {noreply, handle_data(Data, State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal functions

% handle data chunk possibly splitting it into lines
handle_data(Data, State = #state{left = []}) ->
    handle_lines(string:split(Data, "\n", all), State);

% if there was unhandled remainder, add it to the data chunk
handle_data(Data, State = #state{left = Left}) ->
    handle_data(Left ++ Data, State#state{left = []}).

% save remainder if there was no newline at the end of chunk
handle_lines([Str], State) when Str =/= [] ->
    State#state{left = Str};
handle_lines([Str | T], State) ->
    handle_lines(T, handle_line(Str, State));
handle_lines([], State) ->
    State.

% process line, detect command
handle_line("in" ++ Payload, State) ->
    enqueue(Payload, State);
handle_line("out", State) ->
    dequeue(State);
% simply ignore unexpected garbage
handle_line(_, State) ->
    State.

% create empty banker's queue
init_queue(State) ->
    State#state{queue = queue:new()}.

% enqueue payload
enqueue(Data, State = #state{queue = Q}) ->
    State#state{queue = queue:in(Data, Q)}.

% output result (one item, if any)
dequeue(State = #state{queue = Q}) ->
    case queue:out(Q) of
    {{value, Data}, Q1} ->
        send_data(Data ++ "\n", State),
        State#state{queue = Q1};
    {empty, Q} ->
        State
    end.

% send data chunk to client
send_data(Data, #state{sock = Sock}) ->
    case gen_tcp:send(Sock, Data) of
    ok ->
        ok;
    {error, Reason} ->
        error_logger:error_msg("send error: ~p~n", [Reason])
    end.
