-module(wsexample_metrics_publisher).

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/2, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(FETCH_ONLINE_USERS_INTERVAL, 5000).
-define(FETCH_CPU_UTILIZATION_INTERVAL, 1000).

-record(state, {subscribers = [] :: [pid()]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(SubscriberPID, Metric) ->
    gen_server:cast(?SERVER, {subscribe, SubscriberPID, Metric}).

unsubscribe(SubscriberPID) ->
    gen_server:cast(?SERVER, {unsubscribe, SubscriberPID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    %% Sets a periodic task emulating the reception of metrics data.
    TimeStampF = fun() -> erlang:now() end,
    OnlineUsersF = fun() -> random:uniform(1000) end, 
    timer:send_interval(?FETCH_ONLINE_USERS_INTERVAL, 
                        self(), 
                        {metric_info, [<<"online_users">>,
                                       TimeStampF,
                                       OnlineUsersF]}),
    CPUUtilizationF = fun() -> random:uniform(100) end,
    timer:send_interval(?FETCH_CPU_UTILIZATION_INTERVAL,
                       self(),
                       {metric_info, [<<"cpu_utilization">>,
                                      TimeStampF,
                                      CPUUtilizationF]}),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({subscribe, SubscriberPID, Metric}, State) ->
    io:format("[~p] Subscribing ~p to ~p~n", [?MODULE, SubscriberPID, Metric]),
    %% Adds the subscriber to the subscribers list.
    Subscribers = State#state.subscribers ++ [{SubscriberPID, Metric}],
    {noreply, State#state{subscribers = Subscribers}};
handle_cast({unsubscribe, SubscriberPID}, State) ->    
    io:format("[~p] Unsubscribing ~p~n", [?MODULE, SubscriberPID]),
    %% Removes the subscriber from the subscribers list.
    Subscribers = proplists:delete(SubscriberPID, State#state.subscribers),
    {noreply, State#state{subscribers = Subscribers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({metric_info, [Metric, TimeStampF, ValueF]}, State) ->
    TimeStamp = TimeStampF(),
    Value = ValueF(),
    io:format("[~p] Metric info ( ~p ~p ~p )~n", [?MODULE, Metric, TimeStamp, Value]),
    %% Sends the fetched data to all subscribers.
    Subscribers = [Subscriber || {Subscriber, SubsMetric} <- State#state.subscribers, 
                                 SubsMetric =:= Metric],
    lists:foreach(fun(Subscriber) ->
                          io:format("[~p] Sending metric to ~p~n", [?MODULE, Subscriber]),
                          Subscriber ! {metric_info, {TimeStamp, Value}}
                  end,
                 Subscribers),
    {noreply, State};
handle_info({}, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
