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
    error_logger:info_msg("start worker with ~p~n", [Args]),
    gen_server:start_link(?MODULE, Args, []).

-record(state, {
    lsock,
    sock
}).

% gen_server callbacks

init(LSock) ->
    error_logger:info_msg("gonna accept on ~p~n", [LSock]),
    gen_server:cast(self(), accept),
    {ok, #state{lsock = LSock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(accept, State = #state{lsock = LSock}) ->
    case gen_tcp:accept(LSock) of
    {ok, Sock} ->
        error_logger:info_msg("accepted ~p~n", [Sock]),
        % start next acceptor/worker
        qserv_listener:start_worker(),
        % continue serving input stream
        {noreply, State#state{sock = Sock}};
    {error, Reason} ->
        error_logger:error_msg("accept error: ~p~n", [Reason]),
        {stop, normal}
      % gen_server:cast(self(), accept),
      % {noreply, State}
    end;

handle_cast(Msg, State) ->
    io:format("cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info({tcp_closed, Sock}, State = #state{sock = Sock}) ->
    error_logger:info_msg("closing socket ~p~n", [Sock]),
    ok = gen_tcp:close(Sock),
    {stop, normal, #state{}};

handle_info({tcp, Sock, Data}, State = #state{sock = Sock}) ->
    error_logger:info_msg("received ~p~n", [Data]),
    case gen_tcp:send(Sock, Data) of
    ok ->
        ok;
    {error, Reason} ->
        error_logger:error_msg("send error: ~p~n", [Reason])
    end,
    {noreply, State};

handle_info(Info, State) ->
    io:format("info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% internal functions
