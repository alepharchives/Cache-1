%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2012, Jim Rosenblum
%%% @doc Top-level supervisor for jc. 
%%% 
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) ->  {ok,{{supervisor:restart_strategy(), MaxR::non_neg_integer(),
			 MaxT::non_neg_integer()},[supervisor:child_spec()]}}.

init([]) ->
    EvictionManager = {jc_eviction_manager, 
		       {jc_eviction_manager, start_link, []},
		       permanent, 2000, worker, [jc_eviction_manager]},
    SessionManager = {jc_session_manager, 
		       {jc_session_manager, start_link, []},
		       permanent, 2000, worker, [jc_session_manager]},

    Children = [EvictionManager, SessionManager],
    RestartStrategy = {one_for_one, 60, 3600},
    {ok, {RestartStrategy, Children}}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
