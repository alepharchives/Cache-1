%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2012 Jim Rosenblum
%%% @doc The public API for JCache which can be called directly from within 
%%% Erlang applications or via http front-end, {@link //jc_http_server. 
%%% jc_http_server}.
%%%
%%% <ul>
%%% <li>In addition to a {@link value(). Value}, each {@link key(). Key} has an 
%%% associated {@link type(). Type} and {@link scope(). Scope} which are 
%%% arbitrarily named, meta-data tags that allow Keys to be grouped and acted 
%%% upon in ACIDic ways.</li>
%%%
%%% <li>Cache items have an eviction time, expressed in seconds, which controls
%%%     cache eviction, 0 indicates infinity and is the default </li>
%%%
%%% <li>All cache Values should be binary strings. As a convenience, Strings are
%%% converted to their binary equivalent before being persisted. Values are 
%%% returned as binary values.  Keys, Types and Scopes can be any Erlang term.
%%% </li>
%%% 
%%% <li>Create, Update and Inactivate time-stamps (microseconds, unix-style), 
%%% as well as an opaque record reference, are also associated with cache 
%%% entries.</li>
%%%
%%% <li>Inactivate is used to support the change list functions </li>
%%% <li>Change lists are calcuated with respect to a client-supplied 
%%% time-stamp </li>
%%% <ul><li> An inactivated record sets the inactive_tm field</li>
%%%     <li> An inactive record will appear in the deleted list of a change list
%%%          to the extent that its create-time is before the supplied time-
%%%          stamp and its inactive_tm is after </li>
%%%     <li> An inactive record will appear in the updated list of a change list
%%%          to the extent that its create-time is before the supplied time-
%%%          stamp, it is not inactive, and its last_change time is after 
%%%          the supplied time-stamp</li>
%%%     <li> An inactive record will appear in the added list of a change list
%%%          to the extent that its create-time is after the supplied time-
%%%          stamp and it is not inactive</li>
%%%     <li> An inactive record will not appear in a change list if it was
%%%          created and inactivated before the supplied time-stamp </li>
%%%     <li> An inactive record will be removed from the cache after a 
%%%           configurable number of seconds</li>
%%%     <li> An inactive record will, in all other ways, behave like any other
%%%          record until it is deleted</li>
%%% </ul></ul>
%%%
%%% @version {@version}
%%% @end
%%% Created : 16 Oct 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc).

% CRUD API.
-export([
	 add/2, add/3, add/4, add/5, madd/1, madd/2,
	 delete/1, mdelete/1, scope_delete/1, type_delete/1,
	 flush/0, 
	 get/1, mget/1, mget/2, get_scope/1, get_type/1, get_value/1, 
	 replace/2, replace/3, replace/4, replace/5, mreplace/1, mreplace/2,
	 replace_scope/2, replace_type/2, replace_value/2,
	 set/2, set/3, set/4, set/5,  mset/1, mset/2, 
	 scope_set/2, scope_set/3, type_set/2, type_set/3,
	 type_get/1, type_get/2, scope_get/1, scope_get/2
	 ]).


% EXPIRAMENTAL REDIS-STYLE API.
-export([
	 append/2, 
	 decr/1, decrby/2, 
	 incr/1, incrby/2, 
	 getset/2]).

% CHANGE LIST API
-export ([
	  change_list/1, change_lists/1, change_lists/2,
	  scope_change_list/2, type_change_list/2,
	  minactivate/1, minactivate/2,
	  mset_and_inactivate/4
	 ]).



% CACHE META-DATA API
-export([cache_size/0, cache_nodes/0, up/0]).



% definitions of persisted and global records.
-include("../../include/records.hrl").
-include("../../include/types.hrl").


% Ensure that TTLs are valid and alias infinity
-define(VALID_TTL(TTL), is_integer(TTL) andalso TTL >=0).
-define(VALID_TIMESTAMP(TS), is_integer(TS) andalso TS >=0).
-define(INFINITY, 0).




%%==============================================================================
%% META CACHE INFORMATION API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns table size information in records and words.
%% @end
%%------------------------------------------------------------------------------
-spec cache_size() -> {size, [{TableNm::atom(), RecCnt::integer(), Words::integer()}]}.

cache_size()->
    {value, Data} = jc_trx:stats(size),
    {size, Data}.

%%------------------------------------------------------------------------------
%% @doc Returns the date of cluster creation and uptime.
%% @end
%%------------------------------------------------------------------------------
-spec up() -> {uptime, [tuple()]}.

up() ->
    {value, Start} = jc_trx:stats(up),
    StartSecs =  calendar:datetime_to_gregorian_seconds(Start),

    Now =  calendar:now_to_datetime(now()),
    NowSecs =  calendar:datetime_to_gregorian_seconds(Now),

    Uptime = calendar:seconds_to_daystime(NowSecs-StartSecs),

    {uptime, [{up_at, httpd_util:rfc1123_date(Start)},
	      {now, httpd_util:rfc1123_date(Now)},
	      {up_time, Uptime}]}.


%%------------------------------------------------------------------------------
%% @doc Returns all nodes that offer the JCache service.
%% @end
%%------------------------------------------------------------------------------
-spec cache_nodes() -> {resources, [Nodes::atom()]}.

cache_nodes() ->
    case resource_discovery:fetch_resources(?MODULE) of
	{error, _} -> {resources, []};
	Resources  -> Resources
    end.



%%==============================================================================
%% CRUD API
%%==============================================================================    


%%------------------------------------------------------------------------------
%% @doc Associates {@link value(). Value}, {@link type(). Type}, and  
%% {@link scope(). Scope} with {@link key(). Key} <em> if and only if
%% </em> Key doesn't exist. {@link ttl(). TTL} determins Key's expiration in
%% seconds -- 0 indicates infinity.
%% @end ------------------------------------------------------------------------
-spec add(key(), value(), type(), scope(), ttl()) -> {ok, {key, key()}} | 
						     {error, badarg | exists}.

add(Key, Value, Type, Scope, TTL) when ?VALID_TTL(TTL)  ->
    case compress(Value) of 
	{error, _}=E -> E;
	V  -> jc_trx:cache_insert(Key, V, Type, Scope, TTL, add)
    end;
add(_K, _V, _T, _S, _L) ->
    {error, badarg}.

%%------------------------------------------------------------------------------
%% @equiv add(Key, Value, "undefined", "undefined", 0) 
%%@end--------------------------------------------------------------------------
-spec add(key(), value()) -> {ok, {key, key()}} | {error, badarg | exists}.

add(Key, Value) -> 
    add(Key, Value, "undefined", "undefined", ?INFINITY).


%%------------------------------------------------------------------------------
%% @equiv add(Key, Value, "undefined", "undefined", TTL)
%% @end--------------------------------------------------------------------------
-spec add(key(), value(), ttl()) -> {ok, {key, key()}} | {error, badarg} | 
				    {error, exists}.

add(Key, Value, TTL)  ->
    add(Key, Value, "undefined", "undefined", TTL).


%%------------------------------------------------------------------------------
%% @equiv add(Key, Value, Type, Scope, 0)
%% @end--------------------------------------------------------------------------
-spec add(key(), value(), type(), scope()) -> {ok, {key, key()}} | {error, badarg} | {error, exists}.

add(Key, Value, Type, Scope) ->
    add(Key, Value, Type, Scope, ?INFINITY).



%%------------------------------------------------------------------------------
%% @doc Inserts each {@link cache_insert(). Cache Element}, from the supplied 
%% list, that does not already exist, into the cash in one ACID transaction. If 
%% an element does not include a TTL, the supplied {@link ttl(). TTL} parmeter is 
%% used. Returns the keys that were successfully added.
%% @end-------------------------------------------------------------------------
-spec madd([cache_insert()], ttl()) -> {ok, {keys, [key()]}} | {error, bararg}.

madd(ItemList, TTL) when is_list(ItemList), ?VALID_TTL(TTL) -> 
    ValidList = ensure_valid_values(ItemList, []),
    jc_trx:insert_keylist(ValidList, TTL, add);
    
madd(_I, _L) -> 
    {error, badarg}.

%%------------------------------------------------------------------------------
%% @equiv madd(ItemList, 0)
%% @end-------------------------------------------------------------------------
-spec madd([cache_insert()]) -> {ok, {keys, [key()]}} | {error, badarg}.

madd(ItemList) -> 
    madd(ItemList, ?INFINITY).



%%------------------------------------------------------------------------------
%% @doc Deletes the data associated with the {@link key(). Key}. Always returns
%% ok.
%% @end-------------------------------------------------------------------------
-spec delete(key()) -> ok.

delete(Key) -> jc_trx:delete_key(Key).


%%------------------------------------------------------------------------------
%% @doc Deletes the data associated with the list of {@link key(). Keys} in one 
%% ACID transaction. Assuming the arguments are valid, it returns ok.
%% @end-------------------------------------------------------------------------
-spec mdelete([key()]) -> ok | {error, badarg}.

mdelete(Keys) when is_list(Keys) -> 
    jc_trx:delete_keylist(Keys);
mdelete(_) ->
    {error, badarg}.



%%------------------------------------------------------------------------------
%% @doc Deletes all cache items with the given scope.
%% @end-------------------------------------------------------------------------
-spec scope_delete(scope()) ->  ok.

scope_delete(Scope) -> jc_trx:scope_delete(Scope).


%%------------------------------------------------------------------------------
%% @doc Deletes all cache items with the given type.
%% @end-------------------------------------------------------------------------
-spec type_delete(type()) ->  ok.

type_delete(Type) -> jc_trx:type_delete(Type).


%%------------------------------------------------------------------------------
%% @doc Removes all existing items from the cache.
%% @end-------------------------------------------------------------------------
-spec flush() -> ok.

flush() -> jc_trx:flush().



%%------------------------------------------------------------------------------
%% @doc Returns {@link cache_result(). Cache Item} associated with the 
%% supplied {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec get(key()) -> {ok, cache_result()} | {error, not_found}.

get(Key) -> jc_trx:lookup_item(Key).


%%------------------------------------------------------------------------------
%% @doc Returns the list of {@link cache_result(). Cache Items} associated with 
%% the list of {@link key(). Keys} in one ACID transaction.
%% @end-------------------------------------------------------------------------
-spec mget([key()]) -> {ok, [cache_result()]} | {ok, []} |
		       {error, badarg}.

mget(KeyList) ->
    mget(KeyList, 0).


%%------------------------------------------------------------------------------
%% @doc Returns the list of {@link cache_result(). Cache Items} associated with 
%% the list of {@link key(). Keys} that have been inserted, updated or deleted
%% {@link time_stamp(). Since}.
%% @end-------------------------------------------------------------------------
-spec mget([key()], time_stamp() ) -> {ok, [cache_result()]} | {ok, []} | 
				      {error, badarg}.

mget(KeyList, Since) when is_list(KeyList), ?VALID_TIMESTAMP(Since) ->
        jc_trx:lookup_keylist(KeyList, Since);
mget(_, _) -> 
    {error, badarg}.



%%------------------------------------------------------------------------------
%% @equiv scope_get(Scope, 0)
%% @end-------------------------------------------------------------------------
-spec scope_get(scope())-> {ok,[cache_result()]} | {error, not_found}.

scope_get(Scope) -> scope_get(Scope, 0).


%%------------------------------------------------------------------------------
%% @doc Returns the list of {@link cache_result(). Cache Items}, associated with 
%% the supplied {@link scope(). Scope}, in one ACID transaction that have been
%% inserted, updated or deleted {@link time_stamp(). Since}.
%% @end-------------------------------------------------------------------------
-spec scope_get(scope(), time_stamp())-> {ok,[cache_result()]} | 
					 {error, not_found}.

scope_get(Scope, Since) when ?VALID_TIMESTAMP(Since) ->
    jc_trx:lookup_by_scope(Scope, Since).


%%------------------------------------------------------------------------------
%% @doc Returns the list of {@link cache_result(). Cache Items}, associated with 
%% the supplied {@link type(). Type}, in one ACID transaction that have been
%% inserted, updated or deleted {@link time_stamp(). Since}.
%% @end-------------------------------------------------------------------------
-spec type_get(type(), time_stamp())-> {ok, [cache_result()]} | 
				       {error, not_found}.

type_get(Type, Since) when ?VALID_TIMESTAMP(Since) ->
    jc_trx:lookup_by_type(Type, Since).


%%------------------------------------------------------------------------------
%% @equiv type_get(Scope, 0)
%% @end-------------------------------------------------------------------------
-spec type_get(type())-> {ok, [cache_result()]} | {error, not_found}.

type_get(Type) -> type_get(Type, 0).



%%------------------------------------------------------------------------------
%% @doc Returns the {@link value(). Value} for the {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec get_value(key()) -> {ok, {value, value()}} | {error, not_found}.

get_value(Key) ->
    case jc_trx:lookup_item(Key) of
	{ok, {_K, Value, _T, _S, _C, _U, _I, _R}} -> 
	    {ok, {value, Value}};
	{error, not_found} -> 
	    {error, not_found}
    end.


%%------------------------------------------------------------------------------
%% @doc Returns the type for the  {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec get_type(key()) -> {ok, {type, type()}} | {error, not_found}.

get_type(Key) ->
    case jc_trx:lookup_item(Key) of
	{ok, {_K, _V, Type, _S, _C, _U, _I, _R}} -> 
	    {ok, {type, Type}};
	{error, not_found} -> 
	    {error, not_found}
    end.


%%------------------------------------------------------------------------------
%% @doc Returns the scope for the  {@link key(). Key}.
%% @end-------------------------------------------------------------------------
-spec get_scope(key()) -> {ok, {scope, scope()}} | {error, not_found}.

get_scope(Key) ->
    case jc_trx:lookup_item(Key) of
	{ok, {_K, _V, _T, Scope, _C, _U, _I, _R}} -> 
	    {ok, {scope, Scope}};
	{error, not_found} -> 
	    {error, not_found}
    end.



%%------------------------------------------------------------------------------
%% @doc Replaces the data associated with an <em> existing </em> {@link key(). 
%% Key}. Returns {error, not_found} if the Key does not exist.
%% @end--------------------------------------------------------------------------
-spec replace(key(), value(), type(), scope(), ttl()) -> {ok, {key, key()}} | 
							 {error, badarg | 
							  not_found}.

replace(Key, Value, Type, Scope, TTL) when ?VALID_TTL(TTL)-> 
    case compress(Value) of
	{error, _}=E -> E;
	V -> jc_trx:cache_insert(Key, V, Type, Scope, TTL, replace)
    end;
replace(_K,_V,_T,_S,_L) -> 
    {error, badarg}.

%%------------------------------------------------------------------------------
%% @equiv replace(Key, Value, "undefined", "undefined", 0)
%% @end-------------------------------------------------------------------------
-spec replace(key(), value()) -> {ok, {key, key()}} | {error, badarg | 
						       not_found}.

replace(Key, Value) -> 
    replace(Key, Value, "undefined", "undefined", ?INFINITY).


%%------------------------------------------------------------------------------
%% @equiv replace(Key, Value, "undefined", "undefined", TTL)
%% @end-------------------------------------------------------------------------
-spec replace(key(), value(), ttl()) -> {ok, {key, key()}} | {error, badarg |
							      not_found}.

replace(Key, Value, TTL) ->
    replace(Key, Value, "undefined", "undefined", TTL).


%%------------------------------------------------------------------------------
%% @equiv replace(Key, Value, Type, Scope, 0)
%% @end-------------------------------------------------------------------------
-spec replace(key(), value(), type(), scope()) -> {ok, {key, key()}} | 
						  {error, badarg | not_found}.

replace(Key, Value, Type, Scope) -> 
    replace(Key, Value, Type, Scope, ?INFINITY).



%%------------------------------------------------------------------------------
%% @doc Replaces each {@link cache_insert()}, if it already exists, in one
%% ACID transaction. If no {@link ttl(). TTL} is supplied for a particular 
%% {@link cache_insert()} element, the supplied parameter is used. 
%% Returns the keys that were successfully added.
%% @end-------------------------------------------------------------------------
-spec mreplace([cache_insert()], ttl()) -> {ok,{keys,[key()]}} | {error, badarg}.

mreplace(ItemList, TTL) when is_list(ItemList), ?VALID_TTL(TTL) -> 
    Valid = ensure_valid_values(ItemList, []), 
    jc_trx:insert_keylist(Valid, TTL, replace);
mreplace(_I, _L) ->
    {error, badarg}.

%%------------------------------------------------------------------------------
%% @equiv mreplace(ItemList, 0)
%% @end-------------------------------------------------------------------------
-spec mreplace([cache_insert()]) -> {ok, {keys, [key()]}} | {error, badarg}.

mreplace(ItemList) -> 
    mreplace(ItemList, ?INFINITY).


%%------------------------------------------------------------------------------
%% @doc Replace the scope of the given {@link key(). Key} with the supplied
%% parameter if the key exists.
%% @end ------------------------------------------------------------------------
-spec replace_scope(key(), scope()) ->  {ok, {key, key()}} | {error, not_found}.

replace_scope(Key, NewScope) ->
    jc_trx:replace_scope(Key, NewScope).


%%------------------------------------------------------------------------------
%% @doc Replace the type of the given {@link key(). Key} with the supplied
%% type if the key exists.
%% @end ------------------------------------------------------------------------
-spec replace_type(key(), type()) ->  {ok, {key, key()}} | {error, not_found}.

replace_type(Key, NewType) -> 
    jc_trx:replace_type(Key, NewType).


%%------------------------------------------------------------------------------
%% @doc Replace the value of the given {@link key(). Key} with supplied Value
%% if the key exists
%% @end _-----------------------------------------------------------------------
-spec replace_value(key(), value()) ->  {ok, {key, key()}} | {error, badarg | 
							      not_found}.

replace_value(Key, NewValue) ->
    case compress(NewValue) of
	{error, _}=E -> E;
	V -> jc_trx:replace_value(Key, V)
    end.



%%------------------------------------------------------------------------------
%% @doc Identical to {@link jc:add/5} except the {@link key(). Key} is 
%% <em>replaced</em> if it already exists instead of returning an error.
%% @end ------------------------------------------------------------------------
-spec set(key(), value(), type(), scope(), ttl()) -> {ok, {key, key()}} | 
						     {error, badarg}.

set(Key, Value, Type, Scope, TTL)  when ?VALID_TTL(TTL) ->
    case compress(Value) of
	{error, _}=E -> E;
	V -> jc_trx:cache_insert(Key, V, Type, Scope, TTL, set)
    end;
set(_K, _V, _T, _S, _L) ->
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @equiv set(Key, Value, "undefined", "undefined", 0)
%% @end ------------------------------------------------------------------------
-spec set(key(), value()) -> {ok, {key, key()}} | {error, badarg}.

set(Key, Value) -> 
    set(Key, Value, "undefined", "undefined", ?INFINITY).


%%------------------------------------------------------------------------------
%% @equiv set(Key, Value, "undefined", "undefined", TTL)
%% @end ------------------------------------------------------------------------
-spec set(key(), value(), ttl()) -> {ok, {key, key()}} | {error, badarg}.

set(Key, Value, TTL) ->
    set(Key, Value, "undefined", "undefined", TTL).


%%------------------------------------------------------------------------------
%% @equiv set(Key, Value, Type, Scope, 0)
%% @end ------------------------------------------------------------------------
-spec set(key(), value(), type(), scope()) -> {ok, {key, key()}} | 
					      {error, badarg}.

set(Key, Value, Type, Scope) ->
    set(Key, Value, Type, Scope, ?INFINITY).


%%------------------------------------------------------------------------------
%% @equiv mset(ItemList,0)
%% @end-------------------------------------------------------------------------
-spec mset([cache_insert()]) -> {ok, {keys, [key()]}} | {error, badarg}.

mset(ItemList) ->
    mset(ItemList, ?INFINITY).


%%------------------------------------------------------------------------------
%% @doc Inserts each {@link cache_insert()} into the cash in one ACID
%% transaction. <em> Replaces </em> {@link key(). Keys} that already exist. If 
%% an element does not include a TTL, the supplied parameter is used.
%% @end-------------------------------------------------------------------------
-spec mset([cache_insert()], ttl()) -> {ok, {keys, [key()]}} | {error, reason}.

mset(ItemList, TTL) when is_list(ItemList), ?VALID_TTL(TTL) -> 
    Valid = ensure_valid_values(ItemList, []),
    jc_trx:insert_keylist(Valid, TTL, set);
mset(_I, _L) -> 
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Replaces Scope with the given cache-insert elements in one ACID
%% transaction, first <em>DELETING</em> all keys having had the given 
%% Scope. If any of the supplied cache_insert() elements have scope 
%% information, it will be ignored. The function returns all keys keys that 
%% were inserted. 
%% @end-------------------------------------------------------------------------
-spec scope_set(scope(), [cache_insert()]) -> {ok, {keys, [key()]}} | 
					      {error, badarg}.

scope_set(Scope, KeyList) when is_list(KeyList) -> 
    Valid = ensure_valid_values(KeyList,[]),
    jc_trx:set_scope_with(Scope, Valid);
scope_set(_S, _K) -> 
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Same as {@link scope_set/2} except instead of deleteing all keys having 
%% had the given Scope, it replaces their scope with the supplied NewScope.
%% The function returns all keys keys that were inserted. 
%% @end-------------------------------------------------------------------------
-spec scope_set(scope(), [cache_insert()], scope()) -> {ok, {keys, [key()]}} |
						       {error, badarg}.

scope_set(Scope, KeyList, NewScope) when is_list(KeyList)->
    Valid = ensure_valid_values(KeyList,[]),
    jc_trx:set_scope_with(Scope, Valid, NewScope);
scope_set(_S, _K, _NS) -> 
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Replaces Type with the given cache-insert elements in one ACID
%% transaction <em>DELETING</em> all keys having had the given
%% Type. If any of the supplied cache_insert() elements have type information, 
%% it will ignored. The function returns all keys that were inserted. 
%% @end-------------------------------------------------------------------------
-spec type_set(type(), [cache_insert()]) -> [key()] |  {error, badarg}.

type_set(Type, KeyList) when is_list(KeyList) ->
    Valid = ensure_valid_values(KeyList,[]),
    jc_trx:set_type_with(Type, Valid);
type_set(_T, _K) ->
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Same as {@link type_set/2} except instead of deleteing all keys having 
%% had the given Type, it replaces their type to be the supplied NewType.
%% The function returns all keys keys that were inserted. 
%% @end-------------------------------------------------------------------------
-spec type_set(type(), [cache_insert()], type()) -> {ok, {keys, [key()]}} |  {error, badarg}.

type_set(Type, KeyList, NewType) when is_list(KeyList)->
    Valid = ensure_valid_values(KeyList,[]),
    jc_trx:set_type_with(Type, Valid, NewType);
type_set(_S, _K, _NS) -> 
    {error, badarg}.



%%==============================================================================
%%  Advanced operation API -- memcache esque
%%==============================================================================


%%------------------------------------------------------------------------------
%% @doc If the {@link key(). Key} exists, append the supplied string to the
%% end of key's value. If key does not exist, create it and set its value to 
%% the supplied string. EXPERIMENTAL -- may not be in future version.
%% @end ------------------------------------------------------------------------
-spec append(key(), string()) -> {ok, {integer, integer()}} | {error, badarg}.

append(Key, String) -> 
    case compress(String) of
	{error, _}=E -> E;
	S -> jc_trx:append(Key, S)
    end.



%%------------------------------------------------------------------------------
%% @doc Decrement the number stored at {@link key(). Key} by one. If the key 
%% does not exist, create the key with value "-1". EXPERIMENTAL -- may not be 
%% in future version.
%% @end ------------------------------------------------------------------------
-spec decr(key()) -> {ok, {integer, integer()}} | {error, badarg | 
						   value_not_integer}.

decr(Key) -> jc_trx:decrement(Key, 1).


%%------------------------------------------------------------------------------
%% @doc Decrement the number stored at {@link key(). Key} by N. If the key does
%% not exist create the key with string representation of 0 - N. EXPERIMENTAL 
%% -- may not be in future version.
%% @end ------------------------------------------------------------------------
-spec decrby(key(), integer() | string()) -> {ok, {integer, integer()}} | 
					     {error, badarg | value_not_integer}.

decrby(Key, N) when is_integer(N) -> 
    jc_trx:decrement(Key, N);
decrby(Key, N) when is_list(N) -> 
    case catch list_to_integer(N) of
	{'EXIT', _Any} -> {error, badarg};
	Int -> jc_trx:decrement(Key, Int)
    end;
decrby(_K, _D) -> 
    {error, badarg}.


%%------------------------------------------------------------------------------
%% @doc Increment the number stored at {@link key(). Key} by one. If the key 
%% does not exist, create the key with value "1".  EXPERIMENTAL -- may not be 
%% in future version.
%% @end ------------------------------------------------------------------------
-spec incr(key()) -> {ok, {integer, integer()}} | {error, badarg | 
						   value_not_integer}.

incr(Key) -> jc_trx:decrement(Key, -1).


%%------------------------------------------------------------------------------
%% @doc Increment the number stored at {@link key(). key} by increment. If the 
%% key does not exist create the key with string representation of increment.
%% EXPERIMENTAL -- may not be in future version.
%% @end ------------------------------------------------------------------------
-spec incrby(key(), integer() | string()) -> {ok, {integer, integer()}} | 
					    {error, badarg | value_not_integer}.

incrby(Key, N) when is_integer(N) -> 
    jc_trx:decrement(Key, N * -1);
incrby(Key, N) when is_list(N) -> 
    case catch list_to_integer(N) of
	{'EXIT', _Any} -> {error, badarg};
	Int -> jc_trx:decrement(Key, Int * -1)
    end;
incrby(_K, _I) ->
    {error, badarg}.



%%------------------------------------------------------------------------------
%% @doc Replace {@link key(). Key's} value with NewValue and return 
%% OldValue in one atomic operation *if* Key is there. EXPERIMENTAL -- 
%% may not be in future version.
%% @end ------------------------------------------------------------------------
-spec getset(key(), value()) -> {ok, {value, OldValue::value()}} |
				{error, badarg | not_found}.

getset(Key, Value) -> 
    case compress(Value) of
	{error, _}=E -> E;
	V -> jc_trx:getset(Key, V)
    end.



%%==============================================================================
%% CHANGELIST SUPPORT
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Return {@link change_lists()} for the given {@link type(). Types} and 
%% {@link scope(). Scopes} indicated by the Categories paramameter. The change
%% lists are calcuated with respect to {@link time_stamp(). Since}.
%% @end-------------------------------------------------------------------------
-spec change_lists(time_stamp(), [{type, type()} | {scope, scope()}]) -> 
			  {changelists, change_lists()}.

change_lists(Since, Categories) when ?VALID_TIMESTAMP(Since) ->
    F = fun({type, Instance}) ->
		{Instance, type_change_list(Instance, Since)};
	   ({scope, Instance}) ->
		{Instance, scope_change_list(Instance, Since)};
	   (_) ->
		{error, {changelist, {[], [], []}}}
	end,
    ChangeLists = lists:map(F, Categories),
    {changelists, [{Instance, {A, U, D}} ||
		      {Instance, {changelist, {A, U, D}}} <- ChangeLists, 
		      Instance /= error]}.


%%------------------------------------------------------------------------------
%% @equiv change_lists(0, Categories)
%% @end-------------------------------------------------------------------------
-spec change_lists([{type|scope, type() | scope()}]) -> 
			  {changelists, change_lists()}.

change_lists(Categories) ->
    change_lists(0, Categories).


%%------------------------------------------------------------------------------
%% @doc Return the {@link scope(). Scope's} changes {@link time_stamp(). Since}
%% with respect to record creation, updating or inactivating. 
%% Records that are moved out of a scope will result in that record <em>NOT</em>
%% being listed in any of the Adds, Deletes or Updates lists of the source Scope. 
%% Returns {changelist, {Add, Update, Delete}}
%% @end--------------------------------------------------------------------------
-spec scope_change_list(scope(), time_stamp()) -> change_list().

scope_change_list(Scope, Since)  when ?VALID_TIMESTAMP(Since)->
    case jc_trx:lookup_by_scope(Scope, Since) of 
	{ok, Items} -> {changelist, collect(Since, Items, [], [], [])};
	{error, not_found} -> {changelist, {[], [], []}}
    end.


%%-------------------------------------------------------------------------------
%% @doc Equivalent to {@link scope_change_list/2} except with respect to 
%% a given {@link type(). Type}.
%% @end--------------------------------------------------------------------------
-spec type_change_list(type(), time_stamp()) -> change_list().

type_change_list(Type, Since) when ?VALID_TIMESTAMP(Since)->
    case jc_trx:lookup_by_type(Type, Since) of 
	{ok, Items} -> {changelist, collect(Since, Items, [], [], [])};
	{error, not_found} -> {changelist, {[],[],[]}}
    end.



%%-------------------------------------------------------------------------------
%% @doc Equivalent to {@link scope_change_list/2} except with respect to 
%% all keys instead of a given Scope or Type.
%% @end--------------------------------------------------------------------------
-spec change_list(time_stamp()) -> change_list().

change_list(Since) when ?VALID_TIMESTAMP(Since) ->				   
    {ok, Items} = jc_trx:lookup_since(Since),
    {changelist, collect(Since, Items, [], [], [])}.
    


%%------------------------------------------------------------------------------
%% helper function for change_list functions: collects the records into Adds, 
%% Updates and Deletes.
%%
-spec collect(time_stamp(), [cache_result()], [cache_result()], 
	      [cache_result()], [cache_result()]) ->
		     {[cache_result()], [cache_result()], [cache_result()]}.

collect(_, [], Add, Update, Delete) ->
    {Add, Update, Delete};
collect(TimeStamp, [{_K, _V, _T, _S, CT, UT, IT, _} = I|Is], Add, Update, Delete) ->
    case I of
	_ when IT == -1 ->
	    collect(TimeStamp, Is, Add, Update, [I|Delete]);
	_ when IT =/= undefined andalso IT > TimeStamp andalso CT =< TimeStamp->
	    collect(TimeStamp, Is, Add, Update, [I|Delete]);
	_ when IT =/= undefined andalso IT > TimeStamp andalso CT >= TimeStamp->
	    collect(TimeStamp, Is, Add, Update, Delete);
	_ when CT > TimeStamp andalso IT == undefined ->
	    collect(TimeStamp, Is, [I|Add], Update, Delete);
	_ when UT > TimeStamp ->
	    collect(TimeStamp, Is, Add, [I|Update], Delete);
	_ ->
	    collect(TimeStamp, Is, Add, Update, Delete)
end.



%%------------------------------------------------------------------------------
%% @doc Inserts each {@link cache_insert(). Cache Element} from the first list 
%% while inactivating each Key from the second in one ACID transaction. The 
%% first list may have an associated {@link ttl(). TTL}. The delete-list is 
%% inactivated meaning the element's inactivate_tm is populated.
%% The second TTL deptermines when it is *really* evicted (i.e., deleted) from 
%% the cache. 
%% @end-------------------------------------------------------------------------
-spec mset_and_inactivate ([cache_insert()], ttl(), [cache_insert()], ttl()) ->
				  {ok, {keys, []}} | {error, badarg}.

mset_and_inactivate(Sets, STtl, Dels, DTtl) when is_list(Sets),
						 is_list(Dels),
						 ?VALID_TTL(STtl),
						 ?VALID_TTL(DTtl) ->
    Valid = ensure_valid_values(Sets, []),
    jc_trx:mset_and_inactivate(Valid, STtl, Dels, DTtl);

mset_and_inactivate(_S,_ST,_D,_DT) ->
    {error, badarg}.




%%------------------------------------------------------------------------------
%% @doc Set the inactive-flag. Item will be deleted in ttl() seconds.
%% @end-------------------------------------------------------------------------
-spec minactivate([key()], ttl()) -> ok | {error, badarg}.

minactivate(Keys, TTL) when is_list(Keys), ?VALID_TTL(TTL)-> 
    jc_trx:inactivate_keylist(Keys, TTL);
minactivate(_, _) ->
    {error, badarg}.


%% @equiv minactivate(Keys, 0)
-spec minactivate([key()]) -> ok | {error, badarg}.

minactivate(Keys) ->
    minactivate(Keys, ?INFINITY).
	    





%%==============================================================================
%% Utility Functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Return the list of tuples with the value replaced by its binary version if 
%% not already in its binary version.
%% @end
%%------------------------------------------------------------------------------
-spec ensure_valid_values([cache_insert()], []) -> [cache_insert()] | {error, badarg}.

ensure_valid_values([], Result) -> 
    lists:reverse(Result);
ensure_valid_values([T|Ts], Result) -> 
    case compress_tuple(T) of
	{error, badarg} -> ensure_valid_values(Ts, Result);
	Encoded      -> ensure_valid_values(Ts, [Encoded|Result])
    end.
	    
%%------------------------------------------------------------------------------
%% Convert the 2nd term of the tuple (Value) to binary if it isn't already and
%% it's printable. If it is already binary, make sure it is printable.
%%
-spec compress_tuple(tuple()) -> true | {error, badarg}.

compress_tuple(T) when is_tuple(T), size(T) > 1, is_binary(element(2,T)) ->    
    Decoded = binary_to_list(element(2,T)),
    case io_lib:printable_unicode_list(Decoded) of
	true  -> T;
	false -> {error, badarg}
    end;
compress_tuple(T) when is_tuple(T), size(T) > 1-> 
    V = element(2,T),
    case io_lib:printable_unicode_list(V) of
	true  -> setelement(2, T, list_to_binary(V));
	false -> {error, badarg}
    end;
compress_tuple(_) ->
    {error, badarg}.



%%------------------------------------------------------------------------------
%% @doc Ensure the term is printable and convert to binary if necessary
%% @end-------------------------------------------------------------------------
-spec compress(binary() | string()) -> binary() | {error, badarg}.

compress(Value) when is_binary(Value) ->
    Decoded = binary_to_list(Value),
    case io_lib:printable_unicode_list(Decoded) of
	true  -> Value;
	false -> {error, badarg}
    end;
compress(Value) ->
    case io_lib:printable_unicode_list(Value) of
	true  -> list_to_binary(Value);
	false -> {error, badarg}
    end.



