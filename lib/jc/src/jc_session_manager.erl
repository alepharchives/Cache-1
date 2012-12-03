%%%-----------------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc Garbiage collect stale sessions from the session table by removing 
%%% sessions whose last_access is greater than a configured amount of time
%%% @end
%%% Created : 20 Nov 2012 by  <Jim Rosenblum>
%%%-----------------------------------------------------------------------------
-module(jc_session_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 change_pole/1, 
	 change_evict/1]).


%% gen_server callbacks
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).



 % Table and record definitions.
-include("../../include/records.hrl").   
-include("../../include/types.hrl").


-define(SERVER, ?MODULE). 

% how often to look for stale sessions
-define(INTERVAL_S, 3600). 

%stale means last activity older than this
-define(PURGE_S, 3600).    

-record(state, {int_s=?INTERVAL_S, 
		purge_s=?PURGE_S}
       ).


%%%=============================================================================
%%% API
%%%=============================================================================

change_pole(NewInt) when is_integer(NewInt), NewInt >0 ->
    gen_server:cast(?SERVER, {pole, NewInt}).


change_evict(NewInt) when is_integer(NewInt), NewInt >0 ->
    gen_server:cast(?SERVER, {evict, NewInt}).


%%------------------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end-------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server with the polling interval and age limit
%% @end-------------------------------------------------------------------------

init([]) ->
    Interval = jc_util:get_env(jc, session_gc_s, ?INTERVAL_S),
    Purge = jc_util:get_env(jc, session_purge_s, ?PURGE_S),

    {ok, #state{int_s=Interval, purge_s=Purge}, Interval}.



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages: should be none
%% @end-------------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages -- change the sleep interval or the eviction time
%% @end-------------------------------------------------------------------------

handle_cast({pole, Int}, State) ->
    {noreply, State#state{int_s=Int}, Int};

handle_cast({evict, Int}, State) ->
    {noreply, State#state{purge_s=Int}, State#state.int_s};

handle_cast(_Msg, State) ->
    {noreply, State}.



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages -- timeout -> purge and sleep again.
%% @end-------------------------------------------------------------------------

handle_info(timeout, #state{int_s=Int, purge_s=PurgeS}=State) ->
    purge_stale_sessions(PurgeS),
    {noreply, State, Int};

handle_info(_Info, State) ->
    {noreply, State}.



%%------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to terminate.
%% @end-------------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Remove sessions older than PurgeSecs.
%%
purge_stale_sessions(PurgeSecs) ->
    SinceMicroS = jc_util:now_to_Uepoch() - (1000000 * PurgeSecs),
    jc_trx:remove_sessions(SinceMicroS).
