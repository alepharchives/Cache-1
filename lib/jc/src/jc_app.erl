%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim
%%% @doc Application module for the JC application.
%%%
%%% @end
%%% Created : 16 Oct 2011 by Jim <>
%%%-------------------------------------------------------------------
-module(jc_app).

-behaviour(application).

-define(WAIT_FOR_RESOURCES, 2500).
-define(DEFAULT_NODES, [node()]).

%% Application callbacks
-export([start/2, stop/1]).


%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Registers with the resource_discovery process as providing cache 
%% services, notes any other nodes which offer the same service, joins
%% the mnesia cluster (or creates it) and fires-up the top-level 
%% supervisor for the application.
%% @spec start(_StartType, _StartArgs) -> {ok, Pid} |
%%                                        {error, Reason}
%% @end
%%------------------------------------------------------------------------------
-spec start (normal | {takeover | failover, atom()}, [{node, atom()}]) -> 
		    {ok, pid()} | {error, atom()}.

start(_StartType, StartArgs) ->
    DefaultNodes = case StartArgs of
		       [{node, Node}] -> [Node];
		       _ -> ?DEFAULT_NODES
		   end,
    case ensure_contact(DefaultNodes) of 
	ok -> 
	    complete_start();
	{error, _}=Error ->
	    Error
    end.


complete_start() ->
    lager:info("JC Mesh includes: ~p", [node() | nodes()]),
    resource_discovery:add_local_resource(jc, node()),
    resource_discovery:add_target_resource_type(jc),
    resource_discovery:trade_resources(),
    timer:sleep(?WAIT_FOR_RESOURCES),
    jc_store:init(),
    case jc_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever the application has stopped.
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Make sure that we can connect to at least one node and give 
%% some other nodes to join the cluster -- minimizes the chance of
%% not producing one cluster
%% @end
%%------------------------------------------------------------------------------
-spec ensure_contact([atom()]) -> ok | {error, no_contact_nodes}.

ensure_contact(DefaultNodes) ->
    case jc_util:get_env(jc, contact_nodes, DefaultNodes) of
	[] ->
	    {error, no_contact_nodes};
	ContactNodes ->
	    contact(ContactNodes)
    end.

%%------------------------------------------------------------------------------
%%

contact(ContactNodes) ->
    Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
    case Answering of
	[] ->
	    {error, no_contact_nodes};
	_ ->
	    DefaultTime = 6000,
	    WaitTime = jc_util:get_env(jc, wait_time, DefaultTime),
	    wait_for_nodes(length(Answering), WaitTime)
    end.


%%------------------------------------------------------------------------------
%%

wait_for_nodes(MinNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
    case length(nodes()) > MinNodes of
	true ->
	    ok;
	false ->
	    timer:sleep(SliceTime),
	    wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.


