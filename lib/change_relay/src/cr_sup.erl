%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim
%%% @doc Top-level supervisor for change_relay. 
%%% 
%%% @end
%%% Created : 16 Oct 2011 by Jim <>
%%%-----------------------------------------------------------------------------
-module(cr_sup).

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
    ChangeRelay = {change_relay, {change_relay, start_link, []},
		    permanent, 2000, worker, [change_relay]},

    Children = [ChangeRelay],
    RestartStrategy = {one_for_one, 60, 3600},
    {ok, {RestartStrategy, Children}}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
