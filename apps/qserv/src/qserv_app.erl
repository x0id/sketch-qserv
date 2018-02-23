%%%-------------------------------------------------------------------
%% @doc qserv public API
%% @end
%%%-------------------------------------------------------------------

-module(qserv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    case qserv_sup:start_link() of
    {ok, Pid} ->
        % acceptors pool, size may be configuration if needed
        [qserv_listener:start_worker() || _ <- lists:seq(1, 5)],
        {ok, Pid};
    Else ->
        Else
    end.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
