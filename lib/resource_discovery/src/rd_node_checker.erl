%%%-------------------------------------------------------------------
%%% @author Jim Roseblum
%%% @copyright (C) 2011, Jim
%%% @doc Keep the cluster healthy by periodically pinging every node 
%%%      ever seen and asking each node to trade recources. This will 
%%%      restablish contact to nodes that have jittered out, and  will
%%%      ensure that all nodes have synched their resources wanted, 
%%%      offered and found.
%%% @end
%%% Created : 18 Oct 2011 by Jim Rosenblum
%%%-------------------------------------------------------------------
-module(rd_node_checker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(HEART_BEAT, 5000).

-record(state, {clustered_nodes, heart_beat_ms}).

%%%===================================================================
%%% Module API used to start server
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} 
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server and sends the first health-check messag
%%     which initates  resource trading.
%% @spec init([]) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    HeartBeat = get_env(resource_discovery, heart_beat_ms, ?HEART_BEAT),
    erlang:send_after(HeartBeat, ?MODULE, healthcheck),
    {ok, #state{clustered_nodes = [], heart_beat_ms = HeartBeat}}.

	
    
%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages. 
%% @spec handle_call(Request, From, State) -> {reply, ok, State} 
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages. 
%% @spec handle_cast(Msg, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc Ping all nodes ever seen and ask them to trade 
%%      resource information.
%% @spec handle_info(timeout, State) -> {noreply, State} 
%% @end
%%--------------------------------------------------------------------
handle_info(healthcheck, State) ->
    NewClusteredNodes = ping_and_trade(State#state.clustered_nodes),
    HeartBeat = State#state.heart_beat_ms,
    erlang:send_after(HeartBeat, ?MODULE, healthcheck),
    {noreply, State#state{clustered_nodes = NewClusteredNodes}}.
     
    
%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. 
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Ping all connected nodes and all nodes ever connected to (KnownNodes)
%%   and in they have the discovery service, ask them to trade resources.
%% Returns the new list of known nodes.
%% @spec ping_and_trade(KnownNodes) -> UniquNodes
%% @end
%%--------------------------------------------------------------------
ping_and_trade(KnownNodes)->
    ConnectedNodes = [node() | nodes()],
    UniqueNodes = remove_duplicates(lists:append(ConnectedNodes, KnownNodes)),
    Answering  = [Node || Node <- UniqueNodes, 
			  net_adm:ping(Node) =:= pong,
			  has_discovery_service(Node)],

    rpc:eval_everywhere(Answering, 
			resource_discovery, 
			init_found_and_trade_resources, []),
    UniqueNodes.

remove_duplicates(List) ->
    F = fun(Elt, Acc) -> [Elt | lists:delete(Elt, Acc)] end,
    lists:foldl(F, [], List).

has_discovery_service(Node) ->
    is_pid(rpc:call(Node, erlang, whereis, [resource_discovery])).

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.
