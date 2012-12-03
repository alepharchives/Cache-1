%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc A simple Mochiwb server providing a https front-end to JCache. 
%%% All return values are JSON.
%%% All Keys, Values, Types and Scopes must be strings
%%% All Values must be valid json Gets may not return valid Json
%%% Provides Longpolling for certain opperations
%%%
%%% @end
%%% Created : 16 Nov 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(jc_http_server).

-export([start_link/1, stop/0, dispatch_requests/2]).

-define (COMMON_HDR, [{"Content-Type", "application/json;charset=UTF-8"},
		      {"Cache-Control", "no-cache"},
		      {"Pragma", "no-cache"}]).

-define(ZIPPED_HDR(Encoding), [{"Content-Type", "application/json;charset=UTF-8"},
			       {"Cache-Control", "no-cache"},
			       {"Pragma", "no-cache"},
			       {"Vary", "Accept-Encoding"}, 
			       {"Content-Encoding", Encoding}]).
			

%%==============================================================================
%% Module API START OF MOCHIWEB SPECIFIC STUFF
%%==============================================================================
start_link(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun(Req) ->
		   ?MODULE:dispatch_requests(Req, DocRoot)
	   end,
    mochiweb_http:start_link([{name, ?MODULE}, {loop, Loop} | Options1]).
    

stop() ->
     mochiweb_http:stop().


get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%==============================================================================
%% Callback API
%%==============================================================================


%%------------------------------------------------------------------------------
%% Called by mochiweb when a request is received.
%%------------------------------------------------------------------------------
dispatch_requests(Req, DocRoot) ->
    Path = Req:get(path),
    Action = clean_path(Path),
    QS = Req:parse_qs(),

    case Req:get(method) of
	'GET'    ->
	    handle_get(Action, QS, Req, DocRoot);
	'DELETE' ->
	    handle_delete(Action, QS, Req);
	Verb when Verb =='PUT'; Verb == 'POST'    ->
	    Items = get_items(Req),
	    LocalHost = Req:get_header_value('Host'),
	    handle_put(Action, LocalHost, Items, Req);
	_E       ->
	    send_result(Req, {error, bad_action}, 404)
    end.

clean_path(Path) ->
    case string:str(Path, "?") of
	0 ->  Path;
	N -> string:substr(Path, 1, string:len(Path) - (N + 1))
    end.

get_items(Req) -> 
    proplists:get_all_values("item", Req:parse_post()).



%%------------------------------------------------------------------------------
%% Verb handling section. PUT and POST
%%------------------------------------------------------------------------------
construct_name(Name) ->
    list_to_atom("unit_loader-" ++ Name).

handle_put("/refresh/update/unit", _LocalHost, Items, Req) when Items /= [] ->
    [global:send(construct_name(Name), refresh) || Name <-Items],
    
    send_result(Req, ok, 200);

handle_put("/append", LocalHost, Items, Req) when Items /= [] ->
    {Result, Code} = extract_params_and_apply(fun(K, V) -> jc:append(K, V) end, 
					      Items, LocalHost),
    send_result(Req, Result, Code);

handle_put("/increment", LocalHost, Items, Req)  when Items /= [] ->
    {Result, Code} = extract_params_and_apply(fun(K, V) -> jc:incrby(K, V) end, 
					      Items, LocalHost),
    send_result(Req, Result, Code);

handle_put("/decrement", LocalHost, Items, Req)  when Items /= [] ->
    {Result, Code} = extract_params_and_apply(fun(K, V) -> jc:decrby(K, V) end, 
					      Items, LocalHost),
    send_result(Req, Result, Code);

handle_put("/getset", LocalHost, Items, Req)  when Items /= [] ->
    {Result, Code} = extract_params_and_apply(fun(K, V) -> jc:getset(K, V) end, 
					      Items, LocalHost),
    send_result(Req, Result, Code);

handle_put("/replace_scope", LocalHost, Items, Req)  when Items /= [] ->
    {Result, Code} = replace_x_with(scope, LocalHost, Items, replace),
    send_result(Req, Result, Code);

handle_put("/replace_type", LocalHost, Items, Req)  when Items /= [] ->
    {Result, Code} = replace_x_with(type, LocalHost, Items, replace),
    send_result(Req, Result, Code);

handle_put("/replace_value", LocalHost, Items, Req) when Items /= [] ->
    {Result, Code} = replace_x_with(value, LocalHost, Items, replace),
    send_result(Req, Result, Code);

handle_put("/add", LocalHost, Items, Req) ->
    {Result, Code} = insert(add, LocalHost, Items),
    send_result(Req, Result, Code);

handle_put("/replace", LocalHost, Items, Req) ->
    {Result, Code} = insert(replace, LocalHost, Items),
    send_result(Req, Result, Code);

handle_put("/set", LocalHost, Items, Req) ->
    {Result, Code} = insert(set, LocalHost, Items),
    send_result(Req, Result, Code);

handle_put("/scope_set", LocalHost, Items, Req) ->
    Params = Req:parse_post(),
    {Result, Code} = set_x_with(scope, Params, LocalHost, Items),
    send_result(Req, Result, Code);

handle_put("/type_set", LocalHost, Items, Req) ->
    Params = Req:parse_post(),
    {Result, Code} = set_x_with(type, Params, LocalHost, Items),
    send_result(Req, Result, Code);

handle_put(Unknown, _LocalHost, _Items, Req) ->
    send_result(Req, {error, {unknown_action, Unknown}}, 404).




%%------------------------------------------------------------------------------
%% Verb handling section. DELETE
%%------------------------------------------------------------------------------
handle_delete("/flush", _QS, Req) ->
    {Result, Code} = flush(),
    send_result(Req, Result, Code);

handle_delete("/delete/key/" ++ Key, _QS, Req) ->
    {Result, Code} = delete_key(Key),
    send_result(Req, Result, Code);

handle_delete("/delete/keys/" ++ Key, QS, Req) ->
    Keys = [Key] ++ proplists:get_all_values("key", QS),
    {Result, Code} = delete_keys(Keys),
    send_result(Req, Result, Code);

handle_delete("/delete/scope/" ++ Scope, _QS, Req) ->
    {Result, Code} = delete_scope(Scope),
    send_result(Req, Result, Code);

handle_delete("/delete/type/" ++ Type, _QS, Req) ->
    {Result, Code} = delete_type(Type),
    send_result(Req, Result, Code);

handle_delete(Action, _Q, Req) ->
    send_result(Req, {error, {unknown_action, Action}}, 404).


%%------------------------------------------------------------------------------
%% Verb handling section. GET
%%------------------------------------------------------------------------------

handle_get("/", "", Req, _DocRoot) ->
    send_result(Req, all_is_well, 200);

handle_get("/size", _Qs, Req, _) ->
    Result = jc:cache_size(),
    send_result(Req, Result, 200);

handle_get("/resources", _Qs, Req, _) ->
    Result = jc:cache_nodes(),
    send_result(Req, Result, 200);

handle_get("/up", _Qs, Req, _) ->
    Result = jc:up(),
    send_result(Req, Result, 200);


handle_get("/poll/composite/nursing", QS, Req, _) ->
    Unit = proplists:get_value("unit_id", QS, undefined),
    SessionId = extract_session(QS),
    {NewSessionId, Result} = jc:nursing(Unit, SessionId),
    send_result(Req, {{session_id, NewSessionId}, Result}, 200);

handle_get("/poll/composite/bed_management", QS, Req, _) ->
    Unit = proplists:get_value("unit_id", QS, undefined),
    SessionId = extract_session(QS),
    {NewSessionId, Result} = jc:bed_management(Unit, SessionId),
    send_result(Req, {{session_id, NewSessionId}, Result}, 200);

handle_get("/poll/composite/service_planner", QS, Req, _) ->
    {SessionId, Since} = get_from_session_id(QS, "PSP"),
    Result = jc:service_planner(Since),
    send_result(Req, {{session_id, SessionId}, Result}, 200);

handle_get("/poll/composite/evs_supervisor", QS, Req, _) ->
    {SessionId, Since} = get_from_session_id(QS, "EVS"),
    Result = jc:evs(Since),
    send_result(Req, {{session_id, SessionId}, Result}, 200);

handle_get("/poll/composite/case_management", QS, Req, _) ->
    {SessionId, Since} = get_from_session_id(QS, "LOS"),
    Result = jc:los(Since),
    send_result(Req, {{session_id, SessionId}, Result}, 200);

handle_get("/poll/composite/transport_supervisor", QS, Req, _) ->
    {SessionId, Since} = get_from_session_id(QS, "TRX"),
    Result = jc:trx(Since),
    send_result(Req, {{session_id, SessionId}, Result}, 200);


handle_get("/key/scope/" ++ Key, _, Req, _) ->
    {Result, Code} = jc_get_x(scope, Key),
    send_result(Req, Result, Code);
  
handle_get("/key/type/" ++ Key, _, Req, _) ->
    {Result, Code} = jc_get_x(type, Key),
    send_result(Req, Result, Code);

handle_get("/key/value/" ++ Key, _, Req, _) ->
    {Result, Code} = jc_get_x(value, Key),
    send_result(Req, Result, Code);


handle_get("/key/" ++ Key, QS, Req, _) ->
    Since = get_time_stamp(QS),
    {Result, Code} = get_by_keys([Key], Since),
    send_result(Req, Result, Code);

handle_get("/keys/" ++ Key, QS, Req, _) ->
    Keys = [Key|proplists:get_all_values("key", QS)],
    Since = get_time_stamp(QS),
    {Result, Code} = get_by_keys(Keys, Since),
    send_result(Req, Result, Code);


handle_get("/change_list", QS, Req, _) ->
    Since = get_time_stamp(QS),
    Result = jc:change_list(Since),
    send_result(Req, Result, 200);

handle_get("/change_lists", QS, Req, _) ->
    Since = get_time_stamp(QS),
    Topics = [{list_to_atom(T), I} || 
		 {T, I} <- QS, T =:= "type" orelse T =:= "scope"],
    Result = jc:change_lists(Since, Topics),
    send_result(Req, Result, 200);

handle_get("/scope/" ++ Scope, QS, Req, _) ->
    Since = get_time_stamp(QS),
    case jc:scope_change_list(Scope, Since) of
	{changelist, _R}=Result when Scope =/= undefined->     
	    send_result(Req, Result, 200);
	_ -> 
	    send_result(Req, {error, bad_get_params}, 404)
end;

handle_get("/type/" ++ Type, QS, Req, _) ->
    Since = get_time_stamp(QS),

    case jc:type_change_list(Type, Since) of
	{changelist, _R}=Result when Type =/= undefined -> 
	    send_result(Req, Result, 200);
	_ -> 
	    send_result(Req, {error, bad_get_params}, 404)
    end;


% one or more Keys, gets full record back
handle_get("/longpoll/keys/" ++ Key, QS, Req, _) ->
    Keys = [Key] ++ proplists:get_all_values("key", QS),

    Since = get_time_stamp(QS),
    Result = {ok, Found} = jc:mget(Keys, Since),
    case Found of
	[] ->
	    jc_http_lp:init_key_lp(Req, Keys);
	_ ->
	    send_result(Req, Result, 200)
    end;

% gets change list accross all Keys
handle_get("/longpoll/change_list", QS, Req, _) ->
    Since = get_time_stamp(QS),
    case jc:change_list(Since) of
	{changelist, {[],[],[]}} ->
	    jc_http_lp:init_change_list_lp(Req, Since);
	{changelist, _R}=Result ->     
	    send_result(Req, Result, 200)
    end;

% gets change_list for a given scope
handle_get("/longpoll/scope/" ++ Scope, QS, Req, _) ->
    Since = get_time_stamp(QS),

    case jc:scope_change_list(Scope, Since) of
	{changelist, {[],[],[]}} ->
	    jc_http_lp:init_scope_lp(Req, Scope, Since);
	{changelist, _R}=Result ->     
	    send_result(Req, Result, 200)
    end;

handle_get("/longpoll/type/" ++ Type, QS, Req, _) ->
    Since = get_time_stamp(QS),

    case jc:type_change_list(Type, Since) of
	{changelist, {[],[],[]}} ->
	    jc_http_lp:init_type_lp(Req, Type, Since);
	{changelist, _R}=Result ->     
	    send_result(Req, Result, 200)
    end;

handle_get(Unknown, _QS, Req, _) ->    
    send_result(Req, {error, {unknown_action, Unknown}}, 404).



%%==============================================================================
%% END OF MOCHIWEB SPECIFIC STUFF
%%==============================================================================

%-------------------------------------------------------------------------------
% helper functions for PUT / POST
%-------------------------------------------------------------------------------
insert(Semantics, LocalHost, Items)->
    case Items of
	[] ->
	    {{error, bad_put}, 404};
	_ when length(Items) > 1 ->
	    insert_keylist(Items, LocalHost, Semantics);
	_ when length(Items) == 1 ->
	    insert_keylist([Items], LocalHost, Semantics)
    end.

set_x_with(Dimension, Params, LocalHost, Items) ->
    Target = proplists:get_value("target", Params),
    Stash  = proplists:get_value("stash", Params),
    
    XSetFn = x_set_function(Dimension, Stash),

    if 
	length(Items) > 0 ->
	    set_x(Dimension, Items, Target,  XSetFn, LocalHost);
	true -> 
	    {{error, bad_put}, 404}
    end.

x_set_function(type, undefined) -> fun(X, Y) -> jc:type_set(X, Y) end;
x_set_function(type, Stash) -> fun(X, Y) -> jc:type_set(X, Y, Stash) end;
x_set_function(scope, undefined) -> fun(X, Y) -> jc:scope_set(X, Y) end;
x_set_function(scope, Stash) -> fun(X, Y) -> jc:scope_set(X, Y, Stash) end.


%-------------------------------------------------------------------------------
% helper functions for GET
%-------------------------------------------------------------------------------
jc_get_x(Dimension, Key) ->
    case get_item_fn(Dimension, Key) of
	{ok, _R} = Result -> 
	    {Result, 200};
	Error ->  
	    {Error, 200}
    end.

get_item_fn(scope, Key) -> jc:get_scope(Key);
get_item_fn(type, Key) -> jc:get_type(Key);
get_item_fn(value, Key) -> jc:get_value(Key).





%%==============================================================================
%% Functions that interact with JCache
%%==============================================================================
extract_params_and_apply(Jc_Fn, Items, LocalHost) ->
    case catch json_item2jc_args(Items) of 
	{error, bad_json} ->
	    {{error, bad_json}, 400};
	[K, V | _Rest] ->
	   case Jc_Fn(K,V) of
	       {ok, Result} ->
		   URL = make_urls(LocalHost, "/key/", [K]),
		   {{Result, {url, URL}}, 200};
	       Error -> 	  
		   {Error, 400}
	       end
    end.

			    
insert_keylist(Items, LocalHost, Semantics) ->
    case catch json_items2jc_args(Items) of
	{error, bad_json} ->
	    {{error, bad_json}, 400};
	ArgList ->
	    Result = 
		case Semantics of
		    add -> jc:madd(ArgList); 
		    replace -> jc:mreplace(ArgList);
		    set -> jc:mset(ArgList)
		end,
	    Code = case Semantics of 
		       replace -> 200; 
		       _Other  -> 201
		   end,
	    case Result of
		{ok, {keys, []}} ->
		    {{error, not_found}, Code};
		{ok, {keys, KeyList}} ->
		    URLs = make_urls(LocalHost, "/key/", KeyList),
		    {{Semantics, {url, URLs}}, Code};
		Error -> 
		    {Error, 201}
	    end
    end.

replace_x_with(ReplaceType, LocalHost, Item, replace)->
    case catch json_item2jc_args(Item) of 
	{error, bad_json} ->
	    {{error, bad_json}, 400};
	[K, V | _Rest] ->
	    FnReturn = case ReplaceType of
			   scope -> jc:replace_scope(K, V);
			   type  -> jc:replace_type(K,V) ;
			   value -> jc:replace_value(K,V)
		       end,
	    case FnReturn of 
		{ok, Result} ->
		    URL = make_urls(LocalHost, "/key/", [K]),
		    {{Result, {url, URL}}, 200};
		Error ->
		    {Error, 400}
	    end
    end.

set_x(Dimension, Items, Target, XSetFn, LocalHost)->
    case catch json_items2jc_args(Items) of
	{error, bad_json} ->
	    {{error, bad_json}, 400};
	CacheItems ->
	    case XSetFn(Target, CacheItems) of
		{ok, {keys, Result}} ->
		    {Method, Op} = case Dimension of
				       scope -> 
					   {"/scope/", scope_set};
				       type -> 
					   {"/type/", type_set}
				   end,
		    URLs = 
			make_urls(LocalHost, Method, [Target]) ++
			make_urls(LocalHost, "/key/", Result),

		    {{Op, {url, URLs}}, 200};
		Error ->
		    {Error, 400}
	    end
    end.
    

flush() ->
    {jc:flush(), 200}.

delete_keys(Keys) ->
    {jc:mdelete(Keys), 200}.

delete_key(Key) ->
   {jc:delete(Key), 200}.

delete_scope(Key) ->
   {jc:scope_delete(Key), 200}.

delete_type(Key) ->
   {jc:type_delete(Key), 200}.


get_by_keys(Keys, Since) ->
    Result = {ok, Found} = jc:mget(Keys, Since),
    case Found of
	[] ->
	    {{error, not_found}, 200};
	_Found ->
	    {Result, 200}
    end.




%%------------------------------------------------------------------------------
%% @doc Parse the Json passed in via HTTP into K, V, T, S and TTL.
%% Must have a Key and Value, other values are optional.
%% @end-------------------------------------------------------------------------

json_items2jc_args(Items) ->
    [list_to_tuple(json_item2jc_args(E)) || E <- Items].


json_item2jc_args(JsonItem) ->
    {struct, DecodedJson} = 
	case catch mochijson:decode(JsonItem) of
	    {'EXIT', _Reason}->
		throw({error, bad_json});
	    Normal -> Normal
	end,

    MapTargets = fun({Target, Default, ConvertToList}) -> 
		case map_target(Target, DecodedJson, Default, ConvertToList) of 
		    ""     -> throw({error, bad_json});
		    Item -> Item
		end
	end,

    % map the json structure to [k, v, t, s, ttl]
    lists:map(MapTargets, [
			   {"key", "",true}, {"value", "", true}, 
			   {"type", "undefined", false}, 
			   {"scope", "undefined", false},
			   {"ttl", 0, false}]).
    


map_target(Target, TupleList, Default, MakeList) ->
    case lists:keyfind(Target, 1, TupleList) of
	{Target, Value} -> 
	    case MakeList of
		true ->thing_to_list(Value);
		false -> Value
	    end;
	false -> Default
    end.

thing_to_list(Thing) when is_list(Thing) -> Thing;
thing_to_list(Thing) when is_integer(Thing) -> integer_to_list(Thing);
thing_to_list(Thing) when is_float(Thing) -> float_to_list(Thing);
thing_to_list(Thing) when is_atom(Thing) -> atom_to_list(Thing);
thing_to_list(Thing) -> Thing.



%%==============================================================================
%% Functions for sending and formatting results back to clients
%%==============================================================================
send_result(Req, Return, Code) ->
    FResult = json_formatter:format_return(Return),
    success(Req, Code, FResult).


make_urls(LocalHost, Path, ResourceIds) ->
    [lists:flatten([LocalHost, Path, Ri]) || Ri <- ResourceIds].


success(Req, Code, Body) ->
  case iolist_size(Body) > 10240 of
      true ->
	  case accepted_encodings(Req, Body) of
	      no_compress ->
		  Req:respond({Code, ?COMMON_HDR, Body});
	      {Compressed, Encoding} ->
		  Req:respond({Code, ?ZIPPED_HDR(Encoding), Compressed})
	  end;
      false ->
	  Req:respond({Code, ?COMMON_HDR, Body})
  end.




accepted_encodings(Req, Body) ->
    case Req:get_header_value("Accept-Encoding") of
	undefined -> 
	    no_compress;
	MethodList ->
	    Methods = string:tokens(MethodList, ",;"),
	    Deflate = lists:member("deflate", Methods),
	    Zip = lists:member("gzip", Methods),
	    if
		Zip ->
		    {zlib:gzip(Body), "gzip"};
		Deflate ->
		    {zlib:compress(Body), "deflate"};
		true -> 
		    no_compress
		end
    end.



%% return the value of the 'since' URL query paramater. It will either be an
%% integer or 0
%%
get_time_stamp(QS)->
    Seconds = proplists:get_value("since", QS, 0),
    case string:to_integer(Seconds) of
	{Int,[]} -> Int;
	_ -> 0
    end.

%% return the clientSessionId URL query paramater as an integer.
%% 
extract_session(QS)->
    SessionString = proplists:get_value("clientSessionId", QS, "0"),
    case string:to_integer(SessionString) of
	{Int, []} -> Int;
	_ -> 0
    end.


%% extract the session id and time stamp depending on the scope
%%
get_from_session_id(QS, Scope)->
    SessionId = extract_session(QS),
    {OldS, Session, TimeStamp} = jc:get_session(SessionId, Scope),
    Since = case OldS of
		Scope -> TimeStamp;
		_ ->  0
	    end,
    {Session, Since}.


