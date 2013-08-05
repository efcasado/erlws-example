-module(wsexample_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(PORT, 9999).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    wsexample_metrics_publisher:start_link(),
    Dispatch = cowboy_router:compile([{'_', [{"/ws/:metric", wsexample_ws_handler, []}]}]),
    cowboy:start_http(wsexample_app, 100, [{port, ?PORT}], [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
