%%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%%% ex: ts=4 sw=4 ft=erlang et indentexpr=
%%%--------------------------------------------------------------------------
%%% File:    qserv_listener.erl
%%%
%%% @doc     Incoming TCP connection listener
%%% @author  Dmitriy Kargapolov <dmitriy.kargapolov@idt.net>
%%% @since   22 February 2018
%%% @end
%%%-------------------------------------------------------------------------
-module('qserv_listener').
-author('dmitriy.kargapolov@idt.net').

-behaviour(supervisor).

% API exports
-export([start_link/0, start_worker/0]).
-ignore_xref([start_link/0]).

% supervisor callbacks
-export([init/1]).

% external API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker() ->
    supervisor:start_child(?MODULE, []).

% supervisor callbacks

init(_Args) ->
    Port = case application:get_env(port) of
        {ok, Val} -> Val; _ -> 5555 end,
    case gen_tcp:listen(Port, [list, {reuseaddr, true}, {port, Port}]) of
    {ok, Sock} ->
        error_logger:info_msg("listening on port ~p~n", [Port]),
        {ok, {#{strategy => simple_one_for_one}, [#{
            id => qserv_worker,
            start => {qserv_worker, start_link, [Sock]}
        }]}};
    {error, Reason} ->
        error_logger:error_msg("listen error (port ~p): ~p~n", [Port, Reason]),
        ignore
    end.

% internal functions
