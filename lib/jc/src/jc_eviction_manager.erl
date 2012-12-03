%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim
%%% @doc jc_eviction_manager is a server which creates timers which have
%%% a reference to an associated cache entry and a TTL (eviction time).
%%% When the timer goes off, it calls this same server with the eviction message
%%% which then evicts the record in question.
%%% 
%%% This is implemented to understand and be like a generic process so that it 
%%% fits in well with supervisors, appmon, tracing, etc.
%%% @end
%%% Created : 25 Oct 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc_eviction_manager).

-compile([{parse_transform, lager_transform}]).

% api to interact with the module to start, stop and add timers.
-export([start_link/0, start_link/1, add_timer/2, replace_timer/3]).

% api for the call-backs needed to be a "special processes"
-export([system_continue/3, system_terminate/4, write_debug/3]).

% private for initialization.
-export([init/2]).

 % Table and record definitions.
-include("../../include/records.hrl").   
-include("../../include/types.hrl").


%%==============================================================================
%% Module API
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates a timer event and assoicates it with the suppied key_to_value 
%%  record reference. A 0 TTL is ignored as it is the equivolent of  infinity.
%% @end
%%------------------------------------------------------------------------------
-spec add_timer(ttl(), rec_ref()) -> ok.

add_timer(0, _RecRef) ->
    ok;
add_timer(TTL, RecRef) ->
    handle_add_timer(TTL, RecRef),
    ok.


%%------------------------------------------------------------------------------
%% @doc Replaces an existing timer event with a new one.
%% @end
%%------------------------------------------------------------------------------
-spec replace_timer(ttl(), rec_ref(), rec_ref()) -> ok.

replace_timer(0, OldRecRef, _RecRef) ->
    cancel_timer(OldRecRef);
replace_timer(TTL, OldRef, RecRef) ->
    handle_replace_timer(TTL, OldRef, RecRef),
    ok.
    


%%------------------------------------------------------------------------------
%% @doc Starts the server by calling start_link([]) which starts  without 
%% tracing / debugging.
%% @spec  start_link() -> start_link([])
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    start_link([]).


%%------------------------------------------------------------------------------
%% @doc Starts the server with debug options
%% @spec  start_link(Options) -> {ok, Pid} | {error, Reason} where
%%        Options = dbg_opt()
%% @end-------------------------------------------------------------------------
start_link(Options) ->
    case whereis(?MODULE) of
	undefined ->
	    Pid = proc_lib:spawn_link(jc_eviction_manager, 
				      init, 
				      [self(), Options]),
	    register(jc_eviction_manager, Pid),    
	    {ok, Pid};
	Pid ->
	    {error, {already_started, Pid}}
    end.
		

%%==============================================================================
%% jc_eviction_manager process start and main server loop
%%==============================================================================

init(Parent, Options) ->
    process_flag(trap_exit, true),

    Deb = sys:debug_options(Options),
    loop([], Parent, Deb).

loop(State, Parent, Deb) ->
    receive
	{'EXIT', Parent, Reason} ->
            exit(Reason);

	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	% when the timer goes off, it sends an evict

	{evict, RecRef} ->
	    Deb2 = sys:handle_debug(Deb, 
				    {?MODULE, write_debug},
				    ?MODULE, 
				    {in, {evict, RecRef}, self()}),
	    handle_evict(RecRef),
	    Deb3 = sys:handle_debug(Deb2,
				    {?MODULE, write_debug},
				    ?MODULE, 
				    {out, evict, self()}),
	    loop(State, Parent, Deb3);

	{Unknown, RecRef} ->
	    Deb2 = sys:handle_debug(Deb, 
				    {?MODULE, write_debug},
				    ?MODULE, 
				    {in, {unknown, Unknown, RecRef},self()}),
	    unknown(RecRef),
	    Deb3 = sys:handle_debug(Deb2,
				    {?MODULE, write_debug},
				    ?MODULE, 
				    {out, unknown, self()}),
	    loop(State, Parent, Deb3)

    end.


%%===================================================================
%% special-process handling of sys:handle_system_msg and handle_debug
%%===================================================================
system_continue(Parent, Deb, State) ->
    loop(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).



%%===================================================================
%% Timer functions
%%===================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc set a timer
%%------------------------------------------------------------------------------
handle_add_timer(TTLSecs, RecRef) ->
    TTLMsecs = TTLSecs * 1000,
    Ref = erlang:send_after(TTLMsecs, ?MODULE, {evict, RecRef}),
    mnesia:dirty_write(#ttl{key = RecRef, timer_ref = Ref}).
    

handle_replace_timer(TTLSecs, OldRef, RecRef) ->
    lager:debug("Replacing timer, TTL: ~p, Old: ~p, New: ~p",[TTLSecs, OldRef, RecRef]),
    cancel_timer(OldRef),
    handle_add_timer(TTLSecs, RecRef).


%%------------------------------------------------------------------------------
%% @private
%% @doc handle the eviction message
%% @end ------------------------------------------------------------------------
-spec handle_evict(term())-> ok.

handle_evict(RecRef) ->
    evict(RecRef),
    mnesia:dirty_delete({ttl, RecRef}).

cancel_timer(RecRef) ->
    case mnesia:dirty_read({ttl, RecRef}) of 
	[#ttl{timer_ref = T} = Rec] ->
	    lager:debug("Canceling timer ~p",[RecRef]),
	    erlang:cancel_timer(T),
	    mnesia:dirty_delete_object(Rec),
	    ok;
	[] -> ok
    end.


%%------------------------------------------------------------------------------
%% @private
%% @doc handle an eviction
%% @end ------------------------------------------------------------------------
evict(RecRef)->
    jc_trx:delete_record_by_ref(RecRef),
    lager:debug("Evicting Record with Reference:~p",[RecRef]).




%%------------------------------------------------------------------------------
%% @private
%% @doc handle an unrecognized (non-eviction) message. Should never happen.
%% @end ------------------------------------------------------------------------
-spec unknown(reference())-> {aborted,Reason::term()} | {atomic, ResultOfFun::term()}.
unknown(RecRef) ->
    mnesia:dirty_delete({ttl, RecRef}).

