%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc Top-level supervisor for jchs_server. 
%%% 
%%% @end
%%% Created : 16 Oct 2011 by Jim <>
%%%-----------------------------------------------------------------------------
-module(jchs_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link([]) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link([Port]) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).


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
init([Port]) ->
    WebConfig = [{port, Port},
		 {docroot, 
		  local_path(["priv", "www"])}],
    WebServer = {jc_http_server, 
		 {jc_http_server, start_link, [WebConfig]},
		 permanent, 5000, worker, [jc_http_server]},
    Children = [WebServer],
    RestartStrategy = {one_for_one, 60, 3600},
    {ok, {RestartStrategy, Children}}.

local_path(Components) ->
    filename:join([get_base_dir(?MODULE) | Components]).

get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).
