%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2012, Jim Rosenblum
%%% @doc This module manages the mapping of a {@link key(). Key} to 
%%% {@link value(). Value}, {@link type(). Type}, {@link scope(). Scope}.
%%%
%%% <ul>
%%% <li>The Mnesia table, key_to_value, stores the Key, Value, Type, Scope, 
%%%     Create Time, Last Update Time, Inactivate Time and a unique record 
%%%     reference. </li>
%%% <li>The time-stamps are unix-style, time-since-epoch, millisecond 
%%%     granularity.</li>
%%% <li>The ability to calculate change_lists is optimized by having the table 
%%%     be an ordered_set whose key is {LastUpdate, Key}. Key, Type and Scope
%%%     secondary indexes provide fast access to those "keys". </li>
%%% <li>A table, TTL, maps the unique record reference to a timer_ref so that,
%%%     when the timer goes off, it can find the record that needs to be 
%%%     deleted. This is managed by the jc_eviction_manager process </li>
%%% </ul>
%%% 
%%% IT IS UP TO THE CALLER TO WRAP THESE FUNCTIONS IN THE APPROPRIATE MNESIA 
%%% TRANSACTIONS
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc_store).

-compile([{parse_transform, lager_transform}]).

%% Module initialization
-export([init/0]).

%% jc_store API
-export([
	 add/6,
	 delete_cache/0, scope_delete/1, type_delete/1,
	 delete/1, delete_record_by_ref/1, 
	 inactivate_keylist/1,
	 lookup/1, lookup_keylist/1, lookup_keylist/2,
	 lookup_since/1, lookup_by_scope/2, lookup_by_type/2,lookup_by_type_in_scope/3,
	 replace/6, replace_scope/2, replace_type/2, 
	 replace_value/2,
	 set/6
	]).


% Plymouth back-end polling support
-export([write_session/2, touch_session/2, remove_sessions/1]).


 % Table and record definitions.
-include("../../include/records.hrl").   
-include("../../include/types.hrl").

% Time to wait for mnesia tables to initialize
-define(WAIT_FOR_TABLES_MS, 4000). 


%%==============================================================================
%% Module API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Either join an existing mnesia cluster or create the tables and start
%% one.
%% @end-------------------------------------------------------------------------
-spec init() ->ok.

init()->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:start(),
    
    {resources, CacheNodes} = resource_discovery:fetch_resources(jc),
    dynamic_db_init(lists:delete(node(), CacheNodes)).
    


%%==============================================================================
%% Storage service API used by jc_trx
%%==============================================================================


%%==============================================================================
%%                         Inserting and Replacing 
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Insert {@link key(). Key}, {@link value(). Value}, {@link type(). Type},
%% and {@link scope(). Scope} <em> if and only if </em> the Key doesn't exist.
%% {@link ttl(). TTL} determins Key's expiration where 0 is interpreted as 
%% infinity. TTL is in seconds and when that time has elapsed, an eviction 
%% message will be delivered. Reference is a unique identifier for the record 
%% so that the eviction handler can find the record when it needs to evict it.
%% @end-------------------------------------------------------------------------
-spec add(key(),value(),type(),scope(),ttl(),rec_ref()) -> {ok, insert} | 
							   {error, exists}.

add(K, V, T, S, TTL, Ref) ->
    case lookup(K) of
	{error, not_found} ->
	    insert(K, V, T, S, TTL, Ref);
	{ok, _} ->
	    {error, exists}
    end.


%%------------------------------------------------------------------------------
%% @doc Replace the {@link key(). Key's} data with the supplied values <em> if 
%% </em> the key exists; otherwise, return {error, not_found}. Because there 
%% may be a new TTL, return the old record reference so that someone else can 
%% cancel the old eviction if it no longer applies.
%% @end-------------------------------------------------------------------------
-spec replace(key(),value(),type(),scope(),ttl(),rec_ref()) ->
		     {error, not_found} | {ok, {replace, rec_ref()}}.

replace(Key, Value, Type, Scope, TTL, Ref) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[#key_to_value{update_map=UM, ref=OldRef} = Rec] -> 
	    mnesia:delete({key_to_value, UM}),
	    LastUpdate = jc_util:now_to_Uepoch(),

	    % records that were inactivated and are now activated should appear
            % as newly created record to the outside world.
	    NewCreateTime = 
		case Rec#key_to_value.inactive_tm of
		    undefined    -> Rec#key_to_value.create_tm;
		    _Inactivated -> LastUpdate
		end,

	    mnesia:write(Rec#key_to_value{update_map = {LastUpdate, Key},
					  create_tm = NewCreateTime,
					  value = Value,		
					  type  = Type,
					  scope = Scope,
					  last_update = LastUpdate,
					  ttl_secs = TTL,
					  ref = Ref,
					  inactive_tm = undefined}),
	
	    {ok, {replace, OldRef}};
	[] -> 
	    {error, not_found}
    end.


%%------------------------------------------------------------------------------
%% @doc Replace the {@link key(). Key's} {@link scope(). Scope} with the 
%% supplied value <em> if the Key exists </em>; otherwise, return 
%% {error, not_found}. 
%% @end-------------------------------------------------------------------------
-spec replace_scope(key(), scope())-> {ok, {replace, key()}} |
				      {error, not_found}.

replace_scope(Key, Scope) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[#key_to_value{update_map=UM} = Rec] -> 
	    mnesia:delete({key_to_value, UM}),
	    LastUpdate = jc_util:now_to_Uepoch(),		
	    mnesia:write(Rec#key_to_value{update_map = {LastUpdate, Key},
					  scope = Scope, 
					  last_update = LastUpdate}),
	    {ok, {replace, Key}};
	[] ->
	    {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc Replace the {@link key(). Key's} {@link type(). Type} with the 
%% supplied value <em> if the Key exists </em>; otherwise, 
%% return {error, not_found}. 
%% @end-------------------------------------------------------------------------
-spec replace_type(key(), type())-> {ok, {replace, key()}} |
				    {error, not_found}.

replace_type(Key, Type) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[#key_to_value{update_map=UM} = Rec] -> 
	    mnesia:delete({key_to_value, UM}),
	    LastUpdate = jc_util:now_to_Uepoch(),
	    mnesia:write(Rec#key_to_value{update_map = {LastUpdate, Key},
					  type = Type, 
					  last_update = LastUpdate}),		
	    {ok, {replace, Key}};
	[] ->
	    {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc Replace the {@link key(). Key's} {@link value(). Value} with the 
%% supplied value <em> if the Key exists </em>; otherwise, return
%% {error, not_found}. 
%% @end-------------------------------------------------------------------------
-spec replace_value(key(), value())-> {ok, {replace, key()}} |
				      {error, not_found}.

replace_value(Key, Value) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[#key_to_value{update_map=UM} = Rec] -> 
	    mnesia:delete({key_to_value, UM}),
	    LastUpdate = jc_util:now_to_Uepoch(),
	    mnesia:write(Rec#key_to_value{update_map = {LastUpdate, Key},
					  value = Value, 
					  last_update = LastUpdate}),		
	    {ok, {replace, Key}};
	[] ->
	    {error, not_found}
    end.



%%------------------------------------------------------------------------------
%% @doc Set the {@link key(). Key} with the supplied data. Return
%% a tuple indicating whether the Key was replaced or inserted so that higher-
%% level fns that care to know will be able to tell if the opperation resulted
%% in a  newly inserted item or a repalcement.
%% @see add/7.
%% @end-------------------------------------------------------------------------
-spec set(key(), value(), type(),  scope(), ttl(), rec_ref()) -> 
		 {ok, insert} | {ok, {replace, rec_ref()}}.

set(Key, Value, Type, Scope, TTL, Ref) ->
    case replace(Key, Value, Type, Scope,TTL, Ref) of
	{error, not_found} ->
	    insert(Key, Value, Type, Scope, TTL, Ref);
	{ok, {replace, _}}=Result -> Result
    end.



%%------------------------------------------------------------------------------
%% Insert into the cache, ASSUMES KEY DOES NOT ALREADY EXIST. DO NOT USE IF
%% UNSURE AS TO WHETHER THE KEY EXISTS
%%
-spec insert(key(), value(), type(), scope(), ttl(), rec_ref()) ->  
	     {ok, insert} | {transaction, abort}.

insert(Key, Value, Type, Scope, TTL, Ref) ->
    Time = jc_util:now_to_Uepoch(),
    mnesia:write(#key_to_value{update_map={Time, Key},
			       key = Key, 
			       type = Type,
			       scope = Scope,
			       value = Value,
			       create_tm = Time,
			       last_update = Time,
			       ttl_secs = TTL,
			       ref = Ref}),
    {ok, insert}.


		


%%==============================================================================
%%                         Lookup
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Return the #key_to_value{} which contains the data associated with 
%% {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec lookup(key()) -> {ok, #key_to_value{}} | {error, not_found}.

lookup(Key) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[Rec] ->
	    {ok, Rec};
	[] ->
	   {error, not_found}
    end.

%%------------------------------------------------------------------------------
%% @doc Return #key_to_value{} for all items whose last_update is > Since.
%% @end-------------------------------------------------------------------------
-spec lookup_since(time_stamp()) -> {ok, [key_to_value()]}.

lookup_since(Since) ->
    M = [{#key_to_value{update_map={'$1', '_'}, _='_'},
	  [{'>', '$1', Since}], ['$_']}],
    {ok, mnesia:select(key_to_value, M)}.


%%------------------------------------------------------------------------------
%% @doc Return the list of #key_to_value{} for the supplied [{@link key().
%%  Keys}] that have changed since the time indicated by the Since parameter.
%% @end-------------------------------------------------------------------------
-spec lookup_keylist([key()], time_stamp()) -> {ok, [key_to_value()]}.

lookup_keylist([], _) ->
    {ok, []};
lookup_keylist(Keys, Since) ->
    M = [{#key_to_value{update_map={'$1', K}, _='_'},
	  [{'>', '$1', Since}],
	  ['$_']} || K <- Keys],

    {ok, mnesia:select(key_to_value, M)}.


%%------------------------------------------------------------------------------
%% @doc Return the list of #key_to_value{} for the supplied [{@link key(). 
%% Keys}].
%% @end-------------------------------------------------------------------------
-spec lookup_keylist([key()]) -> {ok, [key_to_value()]}.

lookup_keylist([]) ->
    {ok, []};
lookup_keylist(Keys) ->
    F = fun(Key, Acc) ->
		case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
		    [Rec] -> [Rec|Acc];
		    [] -> Acc
		end
	end,
    {ok, lists:foldl(F, [], Keys)}.
	

%%------------------------------------------------------------------------------
%% @doc Return the #key_to_value{} for all cache items with the given 
%% {@link type(). type} whose last_update > Since
%% @end
%%------------------------------------------------------------------------------
-spec lookup_by_type(type(), time_stamp()) -> {ok, [#key_to_value{}]} |
					      {error, not_found}.

lookup_by_type(Type, Since) ->
    M = [{#key_to_value{update_map={'$1', '_'}, type=Type, _='_'},
	  [{'>', '$1', Since}], 
	  ['$_']}],
    case mnesia:select(key_to_value, M) of
	[] -> {error, not_found};
	Rslts -> {ok, Rslts}
    end.



%%------------------------------------------------------------------------------
%% @doc Return the #key_to_value{} for all cache items with the given 
%% {@link type(). types}, that are in the given {@link scope(). scope}, 
%% whose last_update > Since.
%% @end-------------------------------------------------------------------------
-spec lookup_by_type_in_scope(type(), scope(), time_stamp()) -> 
				     {ok, [#key_to_value{}]} | {error, not_found}.

lookup_by_type_in_scope(Type, Scope, Since) ->
    M = [{#key_to_value{update_map={'$1', '_'}, type=T, scope=Scope, _='_'},
	  [{'>', '$1', Since}], 
	  ['$_']} || T<-Type],

    case mnesia:select(key_to_value, M) of
	[] -> {error, not_found};
	Rslts -> {ok, Rslts}
    end.




%%------------------------------------------------------------------------------
%% @doc Return all cache_items with the given {@link type(). type} whose 
%% last_update > Since
%% @end-------------------------------------------------------------------------
-spec lookup_by_scope(scope(), time_stamp()) -> {ok, [key_to_value()]} |
						{error, not_found}.

lookup_by_scope(Scope, Since) ->
    M = [{#key_to_value{update_map={'$1', '_'}, scope=Scope, _='_'},
	  [{'>', '$1', Since}], ['$_']}],
    case mnesia:select(key_to_value, M) of
	[] -> {error, not_found};
	Rslts -> {ok, Rslts}
    end.



%%==============================================================================
%%                         Delete 
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc Delete all cache_items associated with the supplied {@link scope(). 
%% Scope}.
%% @end-------------------------------------------------------------------------
-spec scope_delete(scope()) -> ok.

scope_delete(Scope) ->
    case mnesia:index_read(key_to_value, Scope, #key_to_value.scope) of
	[] ->
	    ok;
	Records ->
	    delete(Records),
	    ok
    end.


%%------------------------------------------------------------------------------
%% @doc Delete all cache_items associated with the supplied {@link type(). 
%% Type}.
%% @end-------------------------------------------------------------------------
-spec type_delete(type()) -> ok.

type_delete(Type) ->
    case mnesia:index_read(key_to_value, Type, #key_to_value.type) of
	[] ->
	    ok;
	Records ->
	    delete(Records),
	    ok
    end. 
				  

%%------------------------------------------------------------------------------
%% @doc Delete all data associated with the {@link key(). Key} or the list of
%% {#key_to_value{}
%% @end-------------------------------------------------------------------------
-spec delete(key()|[key_to_value()]) ->  ok.

delete(Rs) when is_list(Rs), is_record(hd(Rs), key_to_value) ->
    F = fun(#key_to_value{update_map=UM}) ->
		mnesia:delete({key_to_value, UM})
	end,
    lists:foreach(F, Rs),
    ok;

delete(Key) ->
    case mnesia:index_read(key_to_value, Key, #key_to_value.key) of
	[#key_to_value{update_map=UM}] ->
	    mnesia:delete({key_to_value, UM}),
	    ok;
	[] -> ok
	    
    end.


%%------------------------------------------------------------------------------
%% @doc Delete the  entire cache.
%% @end-------------------------------------------------------------------------
-spec delete_cache() ->  ok.

delete_cache() ->
    [mnesia:delete(key_to_value, Akey, sticky_write) || 
	Akey <- mnesia:all_keys(key_to_value)],
    [mnesia:delete(session, Akey, sticky_write) || 
	Akey <- mnesia:all_keys(session)],
    [mnesia:delete(ttl, Akey, sticky_write) || 
	Akey <- mnesia:all_keys(ttl)],

    ok.


%%------------------------------------------------------------------------------
%% @doc Delete the cache element by its record reference. 
%% @end-------------------------------------------------------------------------
-spec delete_record_by_ref(rec_ref()) ->  ok | {error, reason}.

delete_record_by_ref(RecRef) ->
    case mnesia:index_read(key_to_value, RecRef, #key_to_value.ref) of
	[] ->
	    {error, not_found};
	Records ->
	    delete(Records)

    end.



%%==============================================================================
%%                         Changelist support
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc Sets the inactivate_tm to unix-style now for every {@link key(). Key} 
%% in the list.
%% @end-------------------------------------------------------------------------
-spec inactivate_keylist([key()]) -> ok.

inactivate_keylist(KeyList) ->
    F = fun(K) ->
		case mnesia:index_read(key_to_value, K, #key_to_value.key) of
		    [] -> ok;
		    [#key_to_value{update_map=UM} = Rec] ->
			InactiveTime = jc_util:now_to_Uepoch(),
			mnesia:delete({key_to_value, UM}),
			mnesia:write(Rec#key_to_value{update_map={InactiveTime, K},
						      inactive_tm=InactiveTime,
						      last_update=InactiveTime})
		end
	end,

    lists:foreach(F, KeyList),
    ok.
	    

%%==============================================================================
%% For PLYMOUTH clients, provide a clientSessionId that maps between  
%% a unique session and the last retrieve time for a given  plymouth concern 
%% (LOS, EVS, nursing/unit, etc.). These functions aways return 
%% {OldScope, SessionID, Time} where the Time is the last access time for the 
%% given scope.
%%==============================================================================


%%------------------------------------------------------------------------------
%% Create a new session that initially tracks the last view time of the given 
%% scope
%%
write_session(Ref, Scope) ->
    Now = jc_util:now_to_Uepoch(),
    Session = #session{key=Ref, last_scope=Scope, create=Now, last_access=Now},
    mnesia:dirty_write(Session),
    {Scope, Ref, 0}.


%%------------------------------------------------------------------------------
%% If the session doesn't exist, return an error. If it exists replace the Scope 
%% and update the visit time
%%
touch_session(SessionId, NewScope) ->
    case mnesia:dirty_read(session, SessionId) of
	[#session{last_scope=NewScope, last_access=LA} = Session] ->
	    Now = jc_util:now_to_Uepoch(),
	    mnesia:dirty_write(Session#session{last_access=Now}),
	    {NewScope, SessionId, LA};
	[#session{last_scope=OldScope, last_access=LA}=Session] ->
	    Now = jc_util:now_to_Uepoch(),
	    mnesia:dirty_write(Session#session{last_scope=NewScope, last_access=Now}),
	    {OldScope, SessionId, LA};
	[] ->
	    {error, no_session}
    end.							   
    
%%------------------------------------------------------------------------------
%% Look for sessions whose last_access time is older than the given number of 
%% microseconds.
%%
remove_sessions(OlderThanMicroS) ->
    M = [{#session{last_access='$1', _='_'},
	  [{'<', '$1', OlderThanMicroS}], ['$_']}],
    
    case mnesia:dirty_select(session, M) of
	[] -> ok;
	Garbage ->
	    [mnesia:dirty_delete({session, Key}) || 
		#session{key=Key} <-Garbage],
	    ok
    end.


%%==============================================================================
%%                              CLUSTER 
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc If there are existing nodes, add this one to the cluster and copy the
%% needed tables to this  replica. Otherwise, create the tables. 
%% @end-------------------------------------------------------------------------
-spec dynamic_db_init(CacheNodes::[node()]) -> ok.

dynamic_db_init([]) ->
    lager:info("Creating cluster"),

    mnesia:create_table(key_to_value,
			[{attributes, record_info(fields, key_to_value)},
			 {type, ordered_set},
			 {index, [key, ref, type, scope]}
			]),
    mnesia:create_table(ttl,
			[{attributes, record_info(fields, ttl)}
			]),
    mnesia:create_table(session,
			[{attributes, record_info(fields, session)},
			 {type, set}
			]),
    mnesia:create_table(stats,
			[{attributes, record_info(fields, stats)}
			]),

    UpRec = #stats{key = 'jc_store_up_time', 
		   value = calendar:now_to_datetime(now())},
    mnesia:dirty_write(UpRec),
    
    ok;
dynamic_db_init(CacheNodes) ->
    add_extra_nodes(CacheNodes).


-spec add_extra_nodes([node()]) -> ok.

add_extra_nodes([Node|T]) ->
    Wait = jc_util:get_env(jc, table_wait_ms, ?WAIT_FOR_TABLES_MS),
    case mnesia:change_config(extra_db_nodes, [Node]) of
	{ok, [Node]} ->
	    lager:info("Joining cluster: ~p", [node()|nodes()]),
	    mnesia:add_table_copy(schema, node(), ram_copies),
	    mnesia:add_table_copy(key_to_value, node(), ram_copies),
	    mnesia:add_table_copy(session, node(), ram_copies),
	    mnesia:add_table_copy(ttl, node(), ram_copies),
	    mnesia:add_table_copy(stats, node(), ram_copies),
	    Tables = mnesia:system_info(tables),
	    ok = mnesia:wait_for_tables(Tables, Wait);
	_ ->
	    add_extra_nodes(T)
    end.




