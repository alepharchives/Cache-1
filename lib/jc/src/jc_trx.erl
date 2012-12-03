%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011 - 2012, Jim Rosenblum
%%% @doc This module sits between the JCache, pubic, API ({@link jc. JC}) and
%%% the lower-level, Mnesia-interacting {@link jc_store. jc_store}. It provides
%%% transactional support on top of jc_store and implements the higher-level
%%% commands exposed by {@link jc. JC}.
%%%
%%% Eviciton is implemented via timers. Becuase Timers should not be established
%%% within a transaction (because mutiple timers may be established
%%% if a transaction is retried), it is handled here, as opposed to jc_store,
%%% outside of any transaction.
%%%
%%% @end
%%% Created : 16 December 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------

-module(jc_trx).

-compile([{parse_transform, lager_transform}]).

-export([
	 cache_insert/6,  
	 delete_key/1, delete_keylist/1, delete_record_by_ref/1, 
	 scope_delete/1, type_delete/1, flush/0, 
	 inactivate_keylist/2,
	 insert_keylist/3, 
	 lookup_item/1, lookup_keylist/1, lookup_keylist/2, lookup_since/1,
	 lookup_by_scope/1, lookup_by_scope/2, 
	 lookup_by_type/1, lookup_by_type/2,lookup_by_type_in_scope/3,
	 replace_scope/2, replace_type/2, replace_value/2,
	 set_scope_with/2, set_scope_with/3, set_type_with/2, set_type_with/3,
	 mset_and_inactivate/4
	]).

% Plymouth back-end polling support
-export([establish_session/1, 
	 extend_session/2,
	 touch_session/2,
	 remove_sessions/1]).


% CACHE META-DATA SUPPORT
-export([stats/1]).


% EXPIRAMENTAL REDIS-STYLE SUPPORT.
-export([append/2, decrement/2, getset/2]).


% definitions of persisted and global records
-include("../../include/records.hrl").
-include("../../include/types.hrl").


%%------------------------------------------------------------------------------
%% @doc Return JCache information from the stats table.
%% @end ------------------------------------------------------------------------
-spec stats(atom()) -> {value, any()} | {error, not_found | bad_arg}.

stats(size) ->
    Data = 
	[{T, mnesia:table_info(T, size), 
	  mnesia:table_info(T, memory)} || T <-mnesia:system_info(tables)],
    {value, Data};
stats(up) ->
    case mnesia:dirty_read(stats, 'jc_store_up_time') of
	[#stats{value=Value}] ->
	    {value, Value};
	[] ->
	    {error, not_found}
    end;
stats(_) ->
    {error, badarag}.



%%------------------------------------------------------------------------------
%% @doc Adds, Replaces or Sets values into the cache. The Operation parameter
%% dictates which function will be executed.
%% @end ------------------------------------------------------------------------
-spec cache_insert(key(), value(), type(), scope(), ttl(), add|set|replace) ->
			  {ok, {key, key()}} | 
			  {error, not_found | exists}.

cache_insert(Key, Value, Type, Scope,  TTL, Operation) ->
    Ref = make_ref(),
    case store_data(Key, Value, Type, Scope, TTL, Ref, Operation) of
	{ok, {replace, OldRef}} ->
	    lager:debug("~p on ~p and ~p",[Operation, Key, Value]),
	    jc_eviction_manager:replace_timer(TTL, OldRef, Ref),
	    {ok, {key, Key}};
	{ok, insert} ->
	    lager:debug("~p on ~p and ~p",[Operation, Key, Value]),
	    jc_eviction_manager:add_timer(TTL, Ref),
	    {ok, {key, Key}};
	{error, Reason} ->
	    lager:notice("JC Error ~p: ~p on ~p and ~p",[Reason, Operation, Key, Value]),
	    {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% Actually call the jc_store function based on the Operation parameter.
%%
-spec store_data(key(), value(), type(), scope(), 
		 ttl(), rec_ref(), add|set|replace) ->
			{ok, insert} | {ok, {replace, rec_ref()}} | 
			{error, not_found | exists}.

store_data(Key, Value, Type, Scope, TTL, Ref, Operation) ->
    F = fun() ->
		case Operation of
		    add -> jc_store:add(Key, Value, Type, Scope, TTL, Ref);
		    set -> jc_store:set(Key, Value, Type, Scope, TTL, Ref);
		    replace -> jc_store:replace(Key, Value, Type, Scope, 
						       TTL, Ref)
		end
	end,
    transactional_execute(F).




%%------------------------------------------------------------------------------
%% @doc Inactivate all elements in the supplied {@link key(). [Key]} in one ACID 
%% transaction.
%% @end-------------------------------------------------------------------------
-spec inactivate_keylist([key()], ttl()) ->  ok.

inactivate_keylist(Keys, TTL) ->
    lager:debug("Inactivate. TTL: ~p, Keylist: ~p",[TTL, Keys]),
    F = fun() -> jc_store:inactivate_keylist(Keys) end,
    Result = transactional_execute(F),
    case TTL of 
	0 ->
	    Result;
	_ -> 
	    set_inactive_eviction(Keys, TTL),
	    Result
    end.


%%------------------------------------------------------------------------------
%% @doc Have the eviction manager set an eviction for the inactivated item.
%% @end-------------------------------------------------------------------------
-spec set_inactive_eviction([key()], ttl()) -> ok.

set_inactive_eviction(Keys, TTL) ->
    F = fun() ->
		{ok, Results} = jc_store:lookup_keylist(Keys),
		lists:foreach(fun(#key_to_value{ref=Ref}) ->
				      jc_eviction_manager:add_timer(TTL, Ref) 
			      end,
			      Results)
	end,
    
    mnesia:async_dirty(F),
    ok.



%%------------------------------------------------------------------------------
%% @doc Set the elements in SetItems using the supplied TTL. Also Inactivate 
%% the items whose keys are in the InactivateItems list. Those inactivated items
%% will be evicted in the time given by DTtl. These activities are ACIDic.
%% @end -------------------------------------------------------------------------
-spec mset_and_inactivate([cache_insert()], ttl(), [key()], ttl()) ->
				 {ok, {keys, [key()]}}.

mset_and_inactivate(SetItems, STtl, InactItems, DTtl) ->
    F = fun()->
		jc_store:inactivate_keylist(InactItems),
		insert_keylist(SetItems, STtl, set)
	end,
    Result =  transactional_execute(F),

    case DTtl of
	0 ->
	    Result;
	_ ->
	    set_inactive_eviction(InactItems, DTtl),
	    Result
    end.


%%------------------------------------------------------------------------------
%% @doc Delete all data assocated with the supplied {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec delete_key(key()) ->  ok.

delete_key(Key) ->
    F = fun() -> jc_store:delete(Key) end,
    lager:debug("Delete. Key: ~p",[Key]),
    transactional_execute(F),
    ok.


%%------------------------------------------------------------------------------
%% @doc Deletes all elements in the supplied {@link key(). [Key]} in one ACID 
%% transaction.
%% @end-------------------------------------------------------------------------
-spec delete_keylist([key()]) ->  ok.

delete_keylist(Keys) ->
    lager:debug("Delete. Keylist: ~p",[Keys]),
    F = fun() -> [delete_key(K) || K <- Keys] end,
    transactional_execute(F),
    ok.


%%------------------------------------------------------------------------------
%% @doc Delete all cache items with the given scope().
%% @end-------------------------------------------------------------------------
-spec scope_delete(scope()) ->  ok.

scope_delete(Scope) ->
    F = fun() -> jc_store:scope_delete(Scope) end,
    lager:debug("Scope Delete. Scope: ~p",[Scope]),
    transactional_execute(F),
    ok.

%%------------------------------------------------------------------------------
%% @doc Deletes all cache items with the given type().
%% @end-------------------------------------------------------------------------
-spec type_delete(type()) ->  ok.

type_delete(Type) ->
    F = fun() -> jc_store:type_delete(Type) end,
    lager:debug("Type Delete. Type: ~p",[Type]),
    transactional_execute(F),
    ok.
		

%%------------------------------------------------------------------------------
%% @doc Delete the cache element by its record reference. Used by 
%% jc_eviction_manager.
%% @end-------------------------------------------------------------------------
-spec delete_record_by_ref(rec_ref()) ->  ok | {error, reason}.

delete_record_by_ref(RecRef) ->
    F = fun() -> jc_store:delete_record_by_ref(RecRef) end,
    lager:debug("Delete Record by Ref. Ref: ~p",[RecRef]),
    transactional_execute(F),
    ok.


%%------------------------------------------------------------------------------
%% @doc Remove all existing items from the cache.
%% @end-------------------------------------------------------------------------
-spec flush() -> ok.

flush()->
    lager:debug("Flush"),
    F = fun() -> jc_store:delete_cache() end,
    transactional_execute(F).




%%------------------------------------------------------------------------------
%% @doc Insert all of the {@link cache_insert(). Cache} items into the cash in 
%% one ACID transaction.
%% @end-------------------------------------------------------------------------
-spec insert_keylist([cache_insert()], ttl(), 'add'|'replace'|'set') -> 
			    {ok, {keys, [key()]}}.

insert_keylist([], _, _) ->
    {ok, {keys, []}};
insert_keylist(Keys, TTL, Operation) ->
    lager:debug("Insert Keylist. TTL: ~p, ~p on ~p",[TTL, Operation, Keys]),
    ExpandedKeys = expand_keys(Keys, TTL),
    F = fun() -> insert_iterate(ExpandedKeys, [], Operation) end,
    Result = transactional_execute(F),

    case Result of 
	{error, _}=Error ->   Error;
	_                ->   {ok, {keys, Result}}
    end.


%%------------------------------------------------------------------------------
%% Expand a cache tuple into its {K, V, T, S, TT} form using the supplied
%% TTL if the element doesn't supply one.
%%
-spec expand_keys([cache_insert()], ttl()) -> [cache_insert()].

expand_keys(CacheElts, TTL) ->
    F = fun(Elt, Acc)->
		R = case Elt of
			{K,V} ->{K,V,"undefined","undefined", TTL};
			{K,V,KeyTTL} ->{K,V,"undefined", "undefined",KeyTTL};
			{K,V,T,S} ->{K,V,T,S,TTL};
			{K,V,T,S,KeyTTL} -> {K,V,T,S,KeyTTL}
		    end,
		[R|Acc]
	end,
    lists:reverse(lists:foldl(F, [], CacheElts)).


%%------------------------------------------------------------------------------
%% Insert each item in the list, return the list of elements that were 
%% successfully inserted. 
%%
-spec insert_iterate([cache_insert()], [cache_insert()], add|set|replace) ->
			    Successes::[cache_insert()].

insert_iterate([], ResL, _Op) ->
    lists:reverse(ResL);
insert_iterate([{Key, Value, Type, Scope, Ttl} | Ks], ResL, Operation) ->
    case cache_insert(Key, Value, Type, Scope, Ttl, Operation) of
	{error, _R} ->
	    insert_iterate(Ks, ResL, Operation);
	{ok, {key, K}} ->
	    insert_iterate(Ks, [K | ResL], Operation)
    end;
insert_iterate([_Other | Ks], ResL, Operation) ->
    insert_iterate(Ks, ResL, Operation).



%%------------------------------------------------------------------------------
%% @doc Retrieve the data associated with Key.
%% @end-------------------------------------------------------------------------
-spec lookup_item(key()) -> {ok, cache_result()} | {error, not_found}.

lookup_item(Key) ->
    lager:debug("Lookup. Key: ~p",[Key]),
    F = fun() -> jc_store:lookup(Key) end,
    Result = transactional_execute(F),
    case Result of 
	{error, not_found} = E -> E;
	{ok, Res} -> {ok, rec_to_tuple(Res, [])}
    end.


%%------------------------------------------------------------------------------
%% @doc Look up all of the keys in the list in one ACID transaction. 
%% @end-------------------------------------------------------------------------
-spec lookup_keylist([key()])->{ok, [cache_result()]}.

lookup_keylist(Keys)->
    lookup_keylist(Keys, 0).


%%------------------------------------------------------------------------------
%% @doc Return all of the keys in the list in one ACID transaction that have 
%% been created, deleted or updated since {@link time_stamp()}.
%% @end-------------------------------------------------------------------------
-spec lookup_keylist([key()], time_stamp()) -> {ok, [cache_result()]}.

lookup_keylist(Keys, Since)->
    lager:debug("Lookup Keylist. Since: ~p, Keys:~p",[Since, Keys]),
    F = case Since of
	    0 -> fun()  -> jc_store:lookup_keylist(Keys) end;
	    N when N > 0 -> fun() -> jc_store:lookup_keylist(Keys, N) end
	end,
    case transactional_execute(F) of
	{ok, R} -> {ok, rec_to_tuple(R, [])};
	{error,_}=Error -> Error
    end.


%%------------------------------------------------------------------------------
%% @doc Return all keys that have been created, deleted or changed since 
%% {@link time_stamp()}.
%% @end-------------------------------------------------------------------------
-spec lookup_since(time_stamp()) -> {ok, [cache_result()]}.

lookup_since(Since) ->
    F = fun()-> jc_store:lookup_since(Since) end,
    lager:debug("Lookup Since. Since: ~p",[Since]),
    {ok, Results} = transactional_execute(F),
    {ok, rec_to_tuple(Results,[])}.


%%------------------------------------------------------------------------------
%% @equiv lookup_by_type(Type, 0)
%% @end-------------------------------------------------------------------------
-spec(lookup_by_type(type())-> {ok, [cache_result()]} | {error, not_found}).

lookup_by_type(Type)->
    lookup_by_type(Type, 0).


%%------------------------------------------------------------------------------
%% @doc Executes lookup_by_type on all keys with the meta-data type = Type that
%% have been inserited, updated or deleted since {@link time_stamp()}.
%% @end-------------------------------------------------------------------------
-spec(lookup_by_type(type(), time_stamp())-> {ok, [cache_result()]} | 
					     {error, not_found}).

lookup_by_type(Type, Since)->
    F = fun()-> jc_store:lookup_by_type(Type, Since) end,
    lager:debug("Lookup by Type. Type: ~p, Since: ~p",[Type, Since]),
    case transactional_execute(F) of
	{error, _}=E -> E;
	{ok, Results} ->  {ok, rec_to_tuple(Results,[])}
    end.


%%------------------------------------------------------------------------------
%% @doc Executes lookup_by_type_in_scope on all keys with the meta-data type = 
%% Type that have been inserited, updated or deleted since {@link time_stamp()}.
%% that also have the given Scope
%% @end-------------------------------------------------------------------------
-spec lookup_by_type_in_scope(type(), scope(), time_stamp())->
				     {ok, []} | {ok, [cache_result()]}.

lookup_by_type_in_scope(Type, Scope, Since)->
    F = fun() -> jc_store:lookup_by_type_in_scope(Type, Scope, Since) end,
   
    lager:debug("Lookup by Type in Scope. Type: ~p, Scope ~p, Since: ~p",
		[Type, Scope, Since]),

    case transactional_execute(F) of
	{ok, Results} ->
	    {ok, lists:map(fun(R)->rec_to_tuple(R) end, Results)};
	{error, not_found} ->
	    {ok, []}
    end.


%%------------------------------------------------------------------------------
%% @doc Executes lookup_keylist on all keys with meta-data scope = Scope.
%% @end-------------------------------------------------------------------------
-spec lookup_by_scope(scope())-> {ok, [cache_result()]} | {error, not_found}.

lookup_by_scope(Scope) ->
    lookup_by_scope(Scope, 0).

%%------------------------------------------------------------------------------
%% @doc Executes lookup_keylist on all keys with the meta-data scope = Scope
%% that have been inserited, updated or deleted since {@link time_stamp()}.
%% @end-------------------------------------------------------------------------
-spec(lookup_by_scope(scope(), time_stamp())-> {ok, [cache_result()]} | 
					    {error, not_found}).

lookup_by_scope(Scope, Since) ->
    F =  fun() -> jc_store:lookup_by_scope(Scope, Since) end,
    lager:debug("Lookup by Scope. Scope: ~p, Since: ~p",[Scope, Since]),
    case transactional_execute(F) of
	{error, not_found}=E -> 
	    E;
	{ok, Results} ->  
	    {ok, rec_to_tuple(Results,[])}
    end.


		
%%------------------------------------------------------------------------------
%% @doc Replace the Key's scope, if key exists, with the supplied new scope.
%% @end ------------------------------------------------------------------------
-spec replace_scope(key(), scope()) -> {ok, {key, key()}} | {error, not_found}.

replace_scope(Key, NewScope) ->
    lager:debug("Replace Scope: Key ~p, ~p",[Key, NewScope]),
    F = fun() -> jc_store:replace_scope(Key, NewScope) end,
    case transactional_execute(F) of
	{ok, {replace, Key}} ->
	    {ok, {key, Key}};
	Error -> 
	    Error
    end.

%%------------------------------------------------------------------------------
%% @doc Replace all of the Keys scope, if Key exists, with the supplied new 
%% scope.
%% @end ------------------------------------------------------------------------
-spec replace_scope_keylist ([key()], scope()) -> {ok, {key, key()}} | 
						  {error, not_found}.

replace_scope_keylist(Keys, NewScope) ->
    lager:debug("Replace Keylist's Scope. NewScope: ~p, ~p",[NewScope, Keys]),
    F = fun() ->
		[K || K<-Keys, replace_scope(K,NewScope) == {ok, {key, K}}]
	end,
    case transactional_execute(F) of
	{error, not_found} = Error -> 
	    Error;
	Result ->
	    {ok, {keys, Result}}
    end.

%%------------------------------------------------------------------------------
%% @doc Replace the Key's type, if Key exists, with the supplied new type.
%% @end ------------------------------------------------------------------------
-spec replace_type(key(), type()) -> {ok, {key, key()}} | {error, not_found}.

replace_type(Key, NewType) ->
    lager:debug("Replace Type. Key: ~p, NewType: ~p",[Key, NewType]),
    F = fun() -> jc_store:replace_type(Key, NewType) end,
    case transactional_execute(F) of
	{ok, {replace, Key}} ->
	    {ok, {key, Key}};
	{error, not_found} = Error -> 
	    Error
    end.

%%------------------------------------------------------------------------------
%% @doc Replace all of the Key's type, if key exists, with the supplied new type
%% @end ------------------------------------------------------------------------
-spec replace_type_keylist ([key()], type()) -> {ok, {keys, key()}} | 
						{error, not_found}.

replace_type_keylist(Keys, NewType) ->
    lager:debug("Replace Keylists' Type. NewType: ~p, ~p",[NewType, Keys]),
    F = fun() ->
		[K || K<-Keys, replace_type(K, NewType) == {ok, {key, K}}]
	end,
    case transactional_execute(F) of
	{error, not_found} = Error -> 
	    Error;
	Result ->
	    {ok, {keys, Result}}
    end.
    
		      


%%------------------------------------------------------------------------------
%% @doc Replace the Key's value if key exists.
%% @end-------------------------------------------------------------------------
-spec replace_value(key(), value()) -> {ok, {key, key()}} | {error, not_found}.

replace_value(Key, NewValue) ->
    lager:debug("Replace Value. Key: ~p, NewValue: ~p",[Key, NewValue]),
    F = fun() -> jc_store:replace_value(Key, NewValue) end,
    case transactional_execute(F) of
	{ok, {replace, Key}} ->
	    {ok, {key, Key}};
	Error -> 
	    Error
    end.




%%------------------------------------------------------------------------------
%% @doc Delete all cache-elements of the given Scope and then add the supplied
%% cache-elements to the given Scope. If any of the supplied elements have scope
%% information, it will be overwritten by the scope parameter. Returns all 
%% keys that were inserted.
%% @end-------------------------------------------------------------------------
-spec set_scope_with(scope(), [cache_insert()]) -> {ok, {keys, [key()]}}.

set_scope_with(Scope, CacheInserts) ->
    set_scope_with(Scope, CacheInserts, jc_delete_jc).


%%------------------------------------------------------------------------------
%% @doc Same as {@link set_scope_with/2}, but instead of deleting the cache-
%% elements that have the supplied scope, they are reasigned to a "Transfer" 
%% scope, supplied via the TransferScope parameter. If the TransferScope
%% is the atom jc_delete_jc then you get {@link jc_trx:set_scope_with/2}
%% behaviour.
%% @end--------------------------------------------------------------------------
-spec set_scope_with(scope(), [cache_insert()], scope()|delete) -> 
			    {ok, {keys, [key()]}}.

set_scope_with(Scope, CacheInserts, TransferScope) ->
    lager:debug("Set Scope With. Scope: ~p, Stash Scope:~p",
		[Scope, TransferScope]),
    Inserts = adjust_keys_scope(Scope, CacheInserts),
    F = fun() ->
		case TransferScope of
		    jc_delete_jc -> 
			scope_delete(Scope);
		    TrxScope -> 
			case lookup_by_scope(Scope) of
			    {error, not_found} -> ok;
			    {ok, CElts} ->
				Ks = [K || {K, _V, _T, _S, _C, _U, _I, _R} 
					       <- CElts],
				replace_scope_keylist(Ks, TrxScope)
			end
		end,
		insert_keylist(Inserts, 0, set)
	end,
    transactional_execute(F).


%%------------------------------------------------------------------------------
%% expand all cache elements replacing their scope with Scope.
%%
-spec adjust_keys_scope(scope(), [key()])-> [cache_insert()].

adjust_keys_scope(Scope, Elements)->
    F = fun(Elt, Acc) ->
		case Elt of
		    {Key, Value} -> 
			[{Key, Value, "undefined", Scope, 0} | Acc];
		    {Key, Value, TTL} -> 
			[{Key, Value, "undefined", Scope, TTL} | Acc];
		    {Key, Value, Type, _S} -> 
			[{Key, Value, Type, Scope, 0} | Acc];
		    {Key, Value, Type, _S, TTL} -> 
			[{Key, Value, Type, Scope, TTL} | Acc];
		    _Other  -> 
			Acc
		end
	end,
    lists:reverse(lists:foldl(F, [], Elements)).


%%------------------------------------------------------------------------------
%% @doc @see jc_trx:set_scope_with/2
%% @end-------------------------------------------------------------------------
-spec set_type_with(scope(), [cache_insert()]) -> [key()] | {error, badarg}.

set_type_with(Type, CacheInserts) ->
    set_type_with(Type, CacheInserts, jc_delete_jc).


%%------------------------------------------------------------------------------
%% @doc @see jc_trx:set_scope_with/3
%% @end-------------------------------------------------------------------------
set_type_with(Type, CacheInserts, TransferType) ->
    lager:debug("Set Type With. Type: ~p, Stash Type: ~p",[Type, TransferType]),
    Inserts = adjust_keys_type(Type, CacheInserts),
    F = fun() ->
		case TransferType of
		    jc_delete_jc -> 
			type_delete(Type);
		    TrxType -> 
			case lookup_by_type(Type) of
			    {error, not_found} -> ok;
			    {ok, CElts} ->
				Ks = 
				 [K || {K, _V, _T, _S, _C, _U, _I, _} 
					   <- CElts],
				replace_type_keylist(Ks, TrxType)
			end
		end,
		insert_keylist(Inserts, 0, set)
	end,    
    transactional_execute(F).


%%------------------------------------------------------------------------------
%%
adjust_keys_type(Type, Items)->
    F = fun(Elt, Acc) ->
		case Elt of
		    {Key, Value} -> 
			[{Key, Value, Type, "undefined", 0} | Acc];
		    {Key, Value, TTL} -> 
			[{Key, Value, Type, "undefined", TTL} | Acc];
		    {Key, Value, _T, Scope} -> 
			[{Key, Value, Type, Scope, 0} | Acc];
		    {Key, Value, _T, Scope, TTL} -> 
			[{Key, Value, Type, Scope, TTL} | Acc];
		    _Other  -> 
			Acc
		end
	end,
    lists:reverse(lists:foldl(F, [], Items)) .




%%------------------------------------------------------------------------------
%% @doc If the key exists, append the supplied string to the end of key's value.
%% If key does not exist, create it and set its value to the supplied string.
%% @end ------------------------------------------------------------------------
-spec append(key(), binary()) -> {ok, {integer, integer()}} | {error, badarg}.

append(Key, BinString) ->
    lager:debug("Append ~p to Key: ~p",[BinString, Key]),

    F = fun() ->
		case lookup_item(Key) of
		    {error, not_found} ->
			cache_insert(Key, BinString, "undefined", "undefined", 
				     0, add),
			{ok, {integer, size(BinString)}};
		    {ok, {_K, OldValue, _T, _S, _Ti, _It, _R, _}} ->
			NewValue = <<OldValue/binary, BinString/binary>>,
			replace_value(Key, NewValue),
			{ok, {integer, size(NewValue)}}
		end
	end,
    transactional_execute(F).


%%------------------------------------------------------------------------------
%% @doc Replace Key's value with NewValue and return OldValue in one atomic 
%% operation.
%% @end ------------------------------------------------------------------------
-spec getset(key(), value()) -> {error, not_found} | {ok, {value, value()}}.				 

getset(Key, NewValue) ->
    lager:debug("Get-Set. Key: ~p to NewValue: ~p",[Key, NewValue]),
    F = fun() ->
		case lookup_item(Key) of
		    {error, R} ->
			{error, R};
		    {ok, {_K, OldValue, _T, _S, _Ti, _It, _R, _}} ->
			replace_value(Key, NewValue),
			{ok, {value, OldValue}}
		end
	end,
    transactional_execute(F).



%%------------------------------------------------------------------------------
%% @doc Decrement Key by Decrememnt. Create key if needed.
%% @end ------------------------------------------------------------------------
-spec decrement(key(), integer()) -> {ok, {integer, integer()}} | 
				     {error, not_found | badarg}.

decrement(Key, Decrement) ->
    lager:debug("Decrement. Key: ~p by ~p",[Key, Decrement]),
    F = fun() ->
		case lookup_item(Key) of
		    {error, not_found} ->
			Value = 0 - Decrement,
			cache_insert(Key, 
				     list_to_binary(integer_to_list(Value)), 
				     "undefined", "undefined", 0, add),
			{ok, {integer, Value}};
		    {ok, {_K, OldValue, _T, _S, _Ti, _It, _R, _}} ->
			dec_value(Key, binary_to_list(OldValue), Decrement)
		end
	end,
    transactional_execute(F).


%%@private
dec_value(Key, OldValue, Decr) ->
    case string:to_integer(OldValue) of
	{error, _Any} -> 
	    {error, value_not_integer};
	{Int, []} -> 
	    NewValue = Int - Decr,
	    replace_value(Key, list_to_binary(integer_to_list(NewValue))),
	    {ok, {integer, NewValue}};
	{_Int, _Rest} -> 
	    {error, value_not_integer}
    end.

%%------------------------------------------------------------------------------
%% @doc Execute F in the context of a transaction if F is not already
%% executing in the context of a transaction. Avoiding nested transactions
%% @end-------------------------------------------------------------------------
transactional_execute(F) ->
    case mnesia:is_transaction() of
	true ->
	    F();
	false ->
	    case mnesia:transaction(F) of
		{atomic, Result} -> Result;
		{aborted, _Reason}=Error -> Error
	    end
    end.


%%------------------------------------------------------------------------------
%% @doc Convert the record (or list of recrods) to a normalized tuple / list of
%% normalized tuples
%% @end-------------------------------------------------------------------------

-spec rec_to_tuple([key_to_value()] | key_to_value(), [cache_result()]) ->
			  cache_result() | [cache_result()].

rec_to_tuple([], Acc) -> Acc;
rec_to_tuple(#key_to_value{key=K, value=V, type=T, scope=S, create_tm=C, 
			   last_update=L, inactive_tm=I,  ref=R}, _Acc)    ->
    {K, V, T, S, C, L, I, R};
rec_to_tuple([#key_to_value{key=K, value=V, type=T, scope=S, create_tm=C, 
			    last_update=L, inactive_tm=I, ref=R}|Rs], Acc) ->
    rec_to_tuple(Rs, [{K, V, T, S, C, L, I, R}|Acc]).

rec_to_tuple(#key_to_value{key=K, value=V, type=T, scope=S, create_tm=C, 
			   last_update=L, inactive_tm=I,  ref=R})    ->
    {K, V, T, S, C, L, I, R}.





%%==============================================================================
%% For PLYMOUTH clients, provide a clientSessionId that maps between  
%% a unique session and the last retrieve time for a given  plymouth concern 
%% (LOS, EVS, nursing/unit, etc.). These functions aways return {SessionID, Time}
%% where the Time is the last access time for the given scope.
%%==============================================================================


%%------------------------------------------------------------------------------
%% Create a new session whose id is built from now() and initially tracks the 
%% last view time of the given scope
%%
establish_session(Scope)->
    {G, S, M} = now(),
    Ref = (G * 1000000000000) + (S * 1000000) + M,
    jc_store:write_session(Ref, Scope).


%%------------------------------------------------------------------------------
%% SessionID already exists, but a new Scope has been accessed, update the Scope
%% associated with the SesionID and the time
%%
extend_session(SessionId, Scope) ->
    jc_store:write_session(SessionId, Scope).


%%------------------------------------------------------------------------------
%% If the session doesn't exist, create it. If it exists but the given Scope 
%% has not previously been visited, extend the session to include that Scope. 
%% Otherwise update the last_access time.
%%

touch_session(0, Scope) ->
    establish_session(Scope);
touch_session(SessionId, Scope) ->
    case jc_store:touch_session(SessionId, Scope) of
	{error, no_session} ->
	    extend_session(SessionId, Scope);
	Result ->
	    Result
    end.


%%------------------------------------------------------------------------------
%% Remove sessions odler than the given number of microseconds
%%
remove_sessions(OlderThanMicroS)->
    jc_store:remove_sessions(OlderThanMicroS).
