%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc Top-level supervisor for resource_discovery service.
%%%
%%% @end
%%% Created : 19 Oct 2011 by Jim Rosenblum
%%%-------------------------------------------------------------------
-module(rd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    NodeChecker = {rd_node_checker, {rd_node_checker, start_link, []},
		  permanent, 2000, worker, [rd_node_checker]},

    DiscoveryServer = {resource_discovery, {resource_discovery, start_link, []},
		    permanent, 2000, worker, [resource_discovery]},

    Children = [NodeChecker, DiscoveryServer],
    RestartStrategy = {one_for_one, 50, 3600},
    {ok, {RestartStrategy, Children}}.

