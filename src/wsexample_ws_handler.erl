-module(wsexample_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(METRICS_PUBLISHER, metrics_publisher).


%%====================================
%% Cowboy Websocket Handler Callbacks
%%====================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    %% Lets the metrics publisher know that this process is interested in
    %% receiving metrics information, which will be received as Erlang
    %% messages and handled by the websocket_info/3 function.
    {Metric, Req2} = cowboy_req:binding(metric, Req),
    wsexample_metrics_publisher:subscribe(self(), Metric),
    {ok, Req2, undefined_state}.

%% Handles the data received from the Websocket connection.
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

%% Handles data received in the form of Erlang messages.
websocket_info({metric_info, {TimeStamp, Value}}, Req, State) ->
    {MegaSecs, Secs, MicroSecs} = TimeStamp,
    UNIXSeconds = (MegaSecs * 1000000000) + (Secs * 1000) + (MicroSecs div 1000),
    MetricInfo = string:join([integer_to_list(UNIXSeconds), 
                              integer_to_list(Value)], " "),
    {reply, {text, MetricInfo}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    metrics_publisher:unsubscribe(self()),
    ok.
