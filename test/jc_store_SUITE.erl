%%%-------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc
%%% Common test suite for jc_store
%%% @end
%%% Created : 29 Sep 2012 by  <Jim Rosenblum>
%%%-------------------------------------------------------------------
-module(jc_store_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("../lib/include/records.hrl").
-include("../lib/include/types.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds, 60*60}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    mnesia:start(),
    application:start(sasl),
    lager:start(),
    lager:set_loglevel(lager_console_backend, error),
    application:start(resource_discovery),
    application:start(jc),
    error_logger:tty(false),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{jc_store, 
      [], 
      [stats, add, replace, replace_scope, replace_type, replace_value, set,
       lookup, lookup_keylist_inactivate, scope_type_delete, delete, 
       delete_by_ref]},
     {jc, 
      [], 
      [jc_add, jc_madd, jc_delete, jc_get, jc_get_x, jc_replace, 
       jc_set, jc_mset, jc_scope_set, set_scope_stash, memcache,
       s_t_change_list, 
       mset_inactivate]},
     {concurrancy,
      [],
      [concur_test]}
    ].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [{group, jc_store}, {group, jc}, {group, concurrancy}].

 
%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------

    

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
stats(_Config) ->
    {uptime, [{up_at, _}, {now, _}, {up_time, {_,{_,_,_}}}]} =
	jc:up(),

    {resources, [Node]} = jc:cache_nodes(),
    Node = node(),

    {size, Data} = jc:cache_size(),
    {stats, 1, _} = lists:keyfind(stats, 1, Data),
    {ttl, 0, _} = lists:keyfind(ttl, 1, Data),
    {key_to_value, 0, _} = lists:keyfind(key_to_value, 1, Data),

    ok.

add(_Config) ->
    F = fun() ->
		Ref = make_ref(),
		{ok, insert} = jc_store:add("add", "1", type, scope, 0, Ref),
		{ok, #key_to_value{key="add",
				   type=type,
				   scope=scope,
				   value= "1", 
				   create_tm=CT,
				   last_update=CT,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}=Rec}= jc_store:lookup("add"),
		{error, exists} = 
		    jc_store:add("add", "2", type2, scope2, 0, Ref),

		{ok, [Rec]}=jc_store:lookup_by_scope(scope, 0),
		{ok, [Rec]}=jc_store:lookup_by_type(type, 0),
		
		ok
		    
	end,
    {atomic, ok} = mnesia:transaction(F).

replace(_Config) ->
    F = fun() ->
		Ref = make_ref(),
		{ok, insert} = jc_store:add("rep", "1", type, scope, 0, Ref),
		{ok, #key_to_value{create_tm=CT,
				   last_update=UT}} = jc_store:lookup("rep"),
		Ref2 = make_ref(),
		{ok, {replace, Ref}} = 
		    jc_store:replace("rep", "12", type2, scope2, 0, Ref2),
		{ok, #key_to_value{key="rep",
				   type=type2,
				   scope=scope2,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT2,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref2}=Rec1} = jc_store:lookup("rep"),
		true = case UT of
			   UT2 -> false;
			   _ -> true
		       end,
		{ok, [Rec1]}=jc_store:lookup_by_scope(scope2, 0),
		{ok, [Rec1]}=jc_store:lookup_by_type(type2, 0),
		{ok, OldS} = jc_store:lookup_by_scope(scope, 0),
		{ok, OldT} = jc_store:lookup_by_type(type, 0),
		false = lists:keyfind("rep", 2, OldS),
		false = lists:keyfind("rep", 2, OldT),
		
		{error, not_found} = 
		    jc_store:replace("r3", "12", type2, scope2, 0, Ref2),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F).

replace_scope(_Config) ->
    F = fun() ->
		{ok, #key_to_value{key="rep",
				   type=type2,
				   scope=scope2,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}} = jc_store:lookup("rep"),
		{ok, {replace, "rep"}} = jc_store:replace_scope("rep", new_scope),
		{ok, #key_to_value{key="rep",
				   type=type2,
				   scope=new_scope,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT2,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}=Rec} = jc_store:lookup("rep"),

		true = case UT2 > UT of
			   true -> true;
			   false -> false
		       end,
		{ok, [Rec]}=jc_store:lookup_by_scope(new_scope, 0),
		true = case jc_store:lookup_by_scope(scope2, 0) of
			  {error, not_found} -> true;
			  {ok, Lst} ->
			      false = lists:keyfind("rep", 2, Lst)
		end,
		{error, not_found} = 
		    jc_store:replace_scope("rep2", new_scope),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F).

replace_type(_Config) ->
    F = fun() ->
		{ok, #key_to_value{key="rep",
				   type=type2,
				   scope=new_scope,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}} = jc_store:lookup("rep"),
		{ok, {replace, "rep"}} = jc_store:replace_type("rep", new_type),
		{ok, #key_to_value{key="rep",
				   type=new_type,
				   scope=new_scope,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT2,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}=Rec} = jc_store:lookup("rep"),

		true = case UT2 > UT of
			   true -> true;
			   false -> false
		       end,
		{ok, [Rec]}=jc_store:lookup_by_type(new_type, 0),
		true = case jc_store:lookup_by_type(type2, 0) of
			  {error, not_found} -> true;
			  {ok, Lst} ->
			      false = lists:keyfind("rep", 2, Lst)
		end,
		{error, not_found} = 
		    jc_store:replace_type("rep2", new_type),
		ok
	end,

    {atomic, ok} = mnesia:transaction(F).

replace_value(_Config) ->
    F = fun() ->
		{ok, #key_to_value{key="rep",
				   type=new_type,
				   scope=new_scope,
				   value= "12", 
				   create_tm=CT,
				   last_update=UT,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}} = jc_store:lookup("rep"),
		{ok, {replace, "rep"}} = jc_store:replace_value("rep", new_value),
		{ok, #key_to_value{key="rep",
				   type=new_type,
				   scope=new_scope,
				   value= new_value, 
				   create_tm=CT,
				   last_update=UT2,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}} = jc_store:lookup("rep"),

		true = case UT2 > UT of
			   true -> true;
			   false -> false
		       end,
		{error, not_found} = 
		    jc_store:replace_value("rep2", new_value),
		ok
	end,

    {atomic, ok} = mnesia:transaction(F).

set(_Config) ->
    F = fun() ->
		{ok, #key_to_value{key="rep",
				   ref=Ref}} = jc_store:lookup("rep"),
		{ok, {replace, Ref}} = jc_store:set("rep", 
						    "new_value",
						    type,
						    scope,
						    0,
						    make_ref()),
		{ok, insert} = jc_store:set("set",
					    undefined,
					    undefined,
					    "set", 
					    0, 
					    make_ref()),
		ok
	end,

    {atomic, ok} = mnesia:transaction(F).
    
		

lookup(_Config)->
    F = fun() -> 
		Ref = make_ref(),
		{ok, insert} = jc_store:set("insert", 
					    "i-value", 
					    type, 
					    scope, 
					    0,
					    Ref),
		{ok, #key_to_value{key="insert",
				   type=type,
				   scope=scope,
				   value= "i-value", 
				   create_tm=CT,
				   last_update=CT,
				   inactive_tm=undefined, 
				   ttl_secs=0, 
				   ref=Ref}} = jc_store:lookup("insert"),
		{error, not_found} = jc_store:lookup("not there"),
		
		{ok, Lst} = jc_store:lookup_since(0),
		true = lists:keymember("insert", 3, Lst),
		
		{ok, []} = jc_store:lookup_since(CT+1),
		{ok, {replace, "insert"}} = 
		    jc_store:replace_value("insert", new_value),
		{ok, Lst2} = jc_store:lookup_since(CT+1),
		true = lists:keymember("insert", 3, Lst2),
		ok

	end,
    {atomic, ok} = mnesia:transaction(F).

lookup_keylist_inactivate(_Config) ->
    F = fun() ->
		{ok, []} = jc_store:lookup_keylist([], 0),
		{ok, []} = jc_store:lookup_keylist([99,100],0),
		jc_store:add(1,1,type,scope, 0, make_ref()),
		jc_store:add(2,2,type,scope, 0, make_ref()),
		jc_store:add(3,3,type,scope, 0, make_ref()),

		{ok, Lst} = jc_store:lookup_keylist([1,2,3,4], 0),
		3 = length(Lst),
		true = lists:keymember(1, 3, Lst),
		true = lists:keymember(2, 3, Lst),
		true = lists:keymember(3, 3, Lst),

		{ok, #key_to_value{key=1,
				   last_update=CT}} = jc_store:lookup(1),
		{ok, Lst2} = jc_store:lookup_keylist([1,2,3,4], CT-1),
		3 = length(Lst2),

		{ok, []} = jc_store:lookup_keylist([], CT+1),

		{ok, Lst3} = jc_store:lookup_keylist([1,2,3,4]),
		3 = length(Lst3),

		{ok, []} = jc_store:lookup_keylist([]),
		{ok, []} = jc_store:lookup_keylist([99,100]),
		
		ok = jc_store:inactivate_keylist([]),
		ok = jc_store:inactivate_keylist([5]),
		ok = jc_store:inactivate_keylist([1,2,3,4]),
		
		{ok, #key_to_value{key=1, last_update= I1, inactive_tm=  I1}} = 
		    jc_store:lookup(1),
		{ok, #key_to_value{key=2, last_update= I2, inactive_tm=  I2}} = 
		    jc_store:lookup(2),
		{ok, #key_to_value{key=3, last_update= I3, inactive_tm=  I3}} = 
		    jc_store:lookup(3),
		ok
		
	end,
    {atomic, ok} = mnesia:transaction(F).

scope_type_delete(_Config) ->
    F = fun() ->
		ok = jc_store:scope_delete(scope_delete),
		ok = jc_store:type_delete(type_delete),

		jc_store:set(1,1,type_delete,scope_delete, 0, make_ref()),
		jc_store:set(2,2,type_delete,scope_delete, 0, make_ref()),
		jc_store:set(3,3,type_delete,scope_delete, 0, make_ref()),
		{ok, Lst1} = jc_store:lookup_by_scope(scope_delete, 0),
		{ok, Lst2} = jc_store:lookup_by_type(type_delete, 0),
		3 = length(Lst1),
		3 = length(Lst2),


		jc_store:scope_delete(scope_delete),

		
		{error, not_found} = jc_store:lookup_by_scope(scope_delete, 0),
		{error, not_found} = jc_store:lookup_by_type(type_delete, 0),


		jc_store:set(1,1,type_delete,scope_delete, 0, make_ref()),
		jc_store:set(2,2,type_delete,scope_delete, 0, make_ref()),
		jc_store:set(3,3,type_delete,scope_delete, 0, make_ref()),
		{ok, Lst3} = jc_store:lookup_by_scope(scope_delete, 0),
		{ok, Lst4} = jc_store:lookup_by_type(type_delete, 0),
		3 = length(Lst3),
		3 = length(Lst4),
		jc_store:type_delete(type_delete),
		
		{error, not_found} = jc_store:lookup_by_scope(scope_delete, 0),
		{error, not_found} = jc_store:lookup_by_type(type_delete, 0),


		ok
	end,
{atomic, ok} = mnesia:transaction(F).


delete(_Config) ->
    F = fun() ->
		ok = jc_store:delete(not_there_delete),
		ok = jc_store:delete_cache(),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F),
    0 = mnesia:table_info(key_to_value, size),
    0 = mnesia:table_info(ttl, size),
    ok.


delete_by_ref(_Config) ->
    F = fun() ->
		Ref = make_ref(),
		jc_store:set(1,1,type_delete,scope_delete, 0, Ref),
		{ok, #key_to_value{key=1, ref=Ref}} = jc_store:lookup(1),
		jc_store:delete_record_by_ref(Ref),
		{error, not_found} = jc_store:lookup(1),

		{error, not_found} = jc_store:delete_record_by_ref(make_ref()),
		ok
	end,
    {atomic, ok} = mnesia:transaction(F).
		
		
		
jc_add(_Config) ->
    {ok, {key, 1}} = jc:add(1,"1"),
    {ok, {key, 2}} = jc:add(2,"2", 1),
    {ok, {key, 3}} = jc:add(3, "3", t, s),
    {ok, {key, 4}} = jc:add(4, "4", t, s, 1),
    {ok, {key, 5}} = jc:add(5,<<"5">>),
    {error, badarg} = jc:add(6, 6),
    {error, badarg} = jc:add(7, <<6>>),
    {ok, {1, <<"1">>, "undefined", "undefined", _, _, _, _}} = jc:get(1),
    {ok, {2, <<"2">>, "undefined", "undefined", _, _, _, _}}  = jc:get(2),
    {ok, {3, <<"3">>, t, s, _, _, _, _}} = jc:get(3),
    {ok, {4, <<"4">>, t, s, _, _, _, _}} = jc:get(4),
    {ok, {5, <<"5">>, "undefined", "undefined", _, _, _, _}} = jc:get(5),
    timer:sleep(1100),
    {error, not_found} = jc:get(2),
    {error, not_found} = jc:get(4),
    {error, badarg} =    jc:add(1,"1", -2),
    {error, badarg} =    jc:add(1,"1", [12]),
    {error, badarg} =    jc:add(1,"1", s, t, -2),
    {error, exists} =   jc:add(1,"1"),
    {error, exists} =   jc:add(3, "3", t, s, 20),
    ok.


jc_madd(_Config) ->
    {ok, {keys, []}} = jc:madd([{1,"1"}]),
    {ok, {keys, []}} = jc:madd([{madd, 1}, -22]),
    {ok, {keys, [madd]}} = jc:madd([{madd, <<"3">>}]),
    {error, badarg} = jc:madd([{madd3,"1"}], -22),

    {ok, {keys, [11,22,33,44]}} = jc:madd([{11,"11"}, {1,"1"}, {22,"22", 1},
					   {33, "33", t, s},
					   {44, "44", t, s, 1}]),

    {ok, {11, <<"11">>, "undefined", "undefined", _, _, _, _}} = jc:get(11),
    {ok, {22, <<"22">>, "undefined", "undefined", _, _, _, _}}  = jc:get(22),
    {ok, {33, <<"33">>, t, s, _, _, _, _}} = jc:get(33),
    {ok, {44, <<"44">>, t, s, _, _, _, _}} = jc:get(44),
    timer:sleep(1100),
    {error, not_found} = jc:get(22),
    {error, not_found} = jc:get(44),


    {ok, {keys, [111,222,333,444]}} = jc:madd([{111,"111"}, {222,"222", 3},
					   {333, "333", t, s},
					   {444, "444", t, s, 3}], 2),
    {ok, {111, <<"111">>, "undefined", "undefined", _, _, _, _}} = jc:get(111),
    {ok, {222, <<"222">>, "undefined", "undefined", _, _, _, _}} = jc:get(222),
    {ok, {333, <<"333">>, t, s, _, _, _, _}} = jc:get(333),
    {ok, {444, <<"444">>, t, s, _, _, _, _}} = jc:get(444),
    timer:sleep(2100),
    {error, not_found} = jc:get(111),
    {ok, {222, <<"222">>, "undefined", "undefined", _, _, _, _}} = jc:get(222),
    {error, not_found} = jc:get(333),
    {ok, {444, <<"444">>, t, s, _, _, _, _}} = jc:get(444),
    timer:sleep(1100),
    {error, not_found} = jc:get(222),
    {error, not_found} = jc:get(444),
    ok.

jc_delete(_Config) ->
    {ok, {keys, [del1, del2, del3, del4]}} = 
	jc:madd([{del1,"11",delt,dels}, 
		 {del2,"1",delt, dels}, 
		 {del3,"22", delt, dels},
		 {del4, "33", delt, dels}]),
    {ok, Lst} = jc:scope_get(dels),

    true = lists:keymember(del3, 1, Lst),
    true = lists:keymember(del2, 1, Lst),
    true = lists:keymember(del4, 1, Lst),
    true = lists:keymember(del1, 1, Lst),

    ok = jc:delete(del2),

    ok = jc:delete(delnothere1),
    {ok, Lst2} = jc:scope_get(dels),

    true = lists:keymember(del3, 1, Lst2),
    false = lists:keyfind(del2, 1, Lst2),
    true = lists:keymember(del4, 1, Lst2),
    true = lists:keymember(del1, 1, Lst2),

    {error, badarg} = jc:mdelete({test}),
    ok = jc:mdelete([del1, del3]),
    {ok, [{del4, <<"33">>, delt, dels, _, _, _, _}]} = jc:scope_get(dels),

    ok = jc:scope_delete(dels),
    {error, not_found} = jc:scope_get(dels),
    ok = jc:scope_delete(dels),
    {ok, {keys, [del1, del2, del3, del4]}} = 
	jc:madd([{del1,"11",delt,dels}, 
		 {del2,"1",delt, dels}, 
		 {del3,"22", delt, dels},
		 {del4, "33", delt, dels}]),
    ok = jc:type_delete(delt),
    {error, not_found} = jc:scope_get(dels),
    {error, not_found} = jc:type_get(delt),
    
    jc:flush(),
    {size, Data} = jc:cache_size(),
    {stats, 1, _} = lists:keyfind(stats, 1, Data),
    {ttl, 0, _} = lists:keyfind(ttl, 1, Data),
    {key_to_value, 0, _} = lists:keyfind(key_to_value, 1, Data),
    ok.
    
jc_get(_Config) ->
    {ok, {keys, [del1, del2, del3, del4]}} = 
	jc:madd([{del1,"11",delt,dels}, 
		 {del2,"22",delt, dels}, 
		 {del3,"33", delt, dels},
		 {del4, "44", delt, dels}]),

    {ok, Lst} =jc:mget([del1,
			del3,
			del4,
			del50]),


    true = lists:keymember(del1, 1, Lst),
    false = lists:keymember(del50, 1, Lst),
    true = lists:keymember(del4, 1, Lst),
    true = lists:keymember(del3, 1, Lst),
    {ok, [{del1, <<"11">>, delt, dels, Ct1, _, _, _}]} = jc:mget([del1]),
    {ok, [{del4, <<"44">>, delt, dels, Ct3, _, _, _}]} = jc:mget([del4]),
    
    

    jc:replace_value(del1, "111"),
    {ok, [{del1, <<"111">>, delt, dels, Ct1, _, _, _}]} = 
	jc:mget([del1, del2, del3, del4], Ct3),
    {error, badarg} = jc:mget(123),
    ok.

jc_get_x(_Config) ->
    jc:mset([{del1,"11",delt,dels}, 
	     {del2,"22",delt, dels}, 
	     {del3,"33", delt, dels},
	     {del4, "44", delt, dels}]),
    {ok, {value, <<"11">>}} = jc:get_value(del1),
    {error, not_found} = jc:get_value(notthere),

    {ok, {type, delt}} = jc:get_type(del1),
    {error, not_found} = jc:get_type(notthere),

    {ok, {scope, dels}} = jc:get_scope(del1),
    {error, not_found} = jc:get_scope(notthere),
    ok.

jc_replace(_Configure) ->    
    jc:mset([{rep1,"11",delt, dels}, 
	     {rep2,"22",delt, dels}, 
	     {rep3,"33", delt, dels},
	     {rep4, "44", delt, dels}]),

    {ok, {key, rep1}} = jc:replace(rep1, "new1", rept, reps, 0),
    {ok, {key, rep2}} = jc:replace(rep2, "new2", 0),
    {ok, {key, rep3}} = jc:replace(rep3, "new3", somet, somes),
    {ok, {key, rep4}} = jc:replace(rep4, "new4"),


    {ok, {rep1, <<"new1">>, rept, reps, _, _, _, _}} = jc:get(rep1),
    {ok, {rep2, <<"new2">>, "undefined", "undefined", _, _, _, _}} =
	jc:get(rep2),
    {ok, {rep3, <<"new3">>, somet, somes, _, _, _, _}} = jc:get(rep3),
    {ok, {rep4, <<"new4">>, "undefined", "undefined", _, _, _, _}} =
	jc:get(rep4),
    
    {error, not_found} = jc:replace(repnot, "new", rept, reps, 0),
    {error, badarg} = jc:replace(repnot, 123, rept, reps, 0),
    {error, badarg} = jc:replace(repnot, 123, rept, reps, cow),


    jc:mset([{rep1,"11",delt, dels}, 
	     {rep2,"22",delt, dels}, 
	     {rep3,"33", delt, dels},
	     {rep4, "44", delt, dels}]),

    {ok, {keys, [rep1, rep2, rep3, rep4]}} =
	jc:mreplace([{rep1, "new1", rept, reps, 0},
		     {rep2, "new2", 0},
		     {rep3, "new3", somet, somes},
		     {rep4, "new4"},
		     {notthere,"3"},
		     {bad, 3},
		     {stillbad, "3", -2}]),
    {ok, {value, <<"new1">>}} = jc:get_value(rep1),
    {ok, {value, <<"new2">>}} = jc:get_value(rep2),
    {ok, {scope, somes}} = jc:get_scope(rep3),
    {ok, {type, somet}} = jc:get_type(rep3),

    {ok, {keys, [rep1, rep2, rep3, rep4]}} =
	jc:mreplace([{rep1, "new1", rept, reps, 0},
		     {rep2, "new2", 0},
		     {rep3, "new3", somet, somes, 1},
		     {rep4, "new4"},
		     {notthere,"3"},
		     {bad, 3},
		     {stillbad, "3", -2}], 2),
    {ok, {value, <<"new3">>}} = jc:get_value(rep3),
    timer:sleep(1050),
    {error, not_found} = jc:get_value(rep3),
    {ok, {value, <<"new4">>}} = jc:get_value(rep4),
    timer:sleep(1000),
    {error, not_found} = jc:get_value(rep4),
    {ok, {value, <<"new1">>}} = jc:get_value(rep1),

    {error, badarg} = jc:mreplace(not_list),
    
    jc:mset([{rep1,"11",delt, dels}, 
	     {rep2,"22",delt, dels}, 
	     {rep3,"33", delt, dels},
	     {rep4, "44", delt, dels}]),
    {ok, {key, rep1}} = jc:replace_scope(rep1, news),
    {ok, {key, rep2}} = jc:replace_type(rep2, newt),
    {ok, {key, rep3}} = jc:replace_value(rep3, <<"news">>),
    {error, not_found} = jc:replace_scope(boo, news),
    {error, not_found} = jc:replace_type(boo, news),
    {error, not_found} = jc:replace_value(boo, <<"news">>),
    {error, badarg} = jc:replace_value(boo, news),
    jc:set(change, "me", 1),
    jc:replace(change, "me2", 2),
    timer:sleep(1500),
    {ok, {value, <<"me2">>}} = jc:get_value(change),
    timer:sleep(1500),
    {error, not_found} = jc:get_value(change),
    jc:set(change, "me", 500),
    jc:replace(change, "me3"),
    timer:sleep(1500),
    {ok, {value, <<"me3">>}} = jc:get_value(change),

    ok.
    
jc_set(_Config) ->    
    {ok, {key, 1}} = jc:set(1,"1"),
    {ok, {key, 2}} = jc:set(2,"2", 1),
    {ok, {key, 3}} = jc:set(3, "3", t, s),
    {ok, {key, 4}} = jc:set(4, "4", t, s, 1),
    {ok, {key, 5}} = jc:set(5,<<"5">>),
    {error, badarg} = jc:set(6, 6),
    {error, badarg} = jc:set(7, <<6>>),
    {ok, {1, <<"1">>, "undefined", "undefined", _, _, _, _}} = jc:get(1),
    {ok, {2, <<"2">>, "undefined", "undefined", _, _, _, _}}  = jc:get(2),
    {ok, {3, <<"3">>, t, s, _, _, _, _}} = jc:get(3),
    {ok, {4, <<"4">>, t, s, _, _, _, _}} = jc:get(4),
    {ok, {5, <<"5">>, "undefined", "undefined", _, _, _, _}} = jc:get(5),
    timer:sleep(1100),
    {error, not_found} = jc:get(2),
    {error, not_found} = jc:get(4),
    {error, badarg} =    jc:set(1,"1", -2),
    {error, badarg} =    jc:set(1,"1", [12]),
    {error, badarg} =    jc:set(1,"1", s, t, -2),
    {ok, {key, 1}} =   jc:set(1,"1"),
    {ok, {key, 3}} =   jc:set(3, "3", t, s),
    ok.

    
    

    
jc_mset(_Config) ->
    {ok, {keys, [1]}} = jc:mset([{1,"1"}]),
    {ok, {keys, []}} = jc:mset([{madd, 1}, -22]),
    {ok, {keys, [madd]}} = jc:mset([{madd, <<"3">>}]),
    {error, badarg} = jc:mset([{madd3,"1"}], -22),

    {ok, {keys, [11,1, 22,33,44]}} = jc:mset([{11,"11"}, {1,"1"}, {22,"22", 1},
					      {33, "33", t, s},
					      {44, "44", t, s, 1}]),

    {ok, {11, <<"11">>, "undefined", "undefined", _, _, _, _}} = jc:get(11),
    {ok, {22, <<"22">>, "undefined", "undefined", _, _, _, _}}  = jc:get(22),
    {ok, {33, <<"33">>, t, s, _, _, _, _}} = jc:get(33),
    {ok, {44, <<"44">>, t, s, _, _, _, _}} = jc:get(44),
    timer:sleep(1100),
    {error, not_found} = jc:get(22),
    {error, not_found} = jc:get(44),


    {ok, {keys, [111,222,333,444]}} = jc:mset([{111,"111"}, {222,"222", 3},
					   {333, "333", t, s},
					   {444, "444", t, s, 3}], 2),
    {ok, {111, <<"111">>, "undefined", "undefined", _, _, _, _}} = jc:get(111),
    {ok, {222, <<"222">>, "undefined", "undefined", _, _, _, _}} = jc:get(222),
    {ok, {333, <<"333">>, t, s, _, _, _, _}} = jc:get(333),
    {ok, {444, <<"444">>, t, s, _, _, _, _}} = jc:get(444),
    timer:sleep(2100),
    {error, not_found} = jc:get(111),
    {ok, {222, <<"222">>, "undefined", "undefined", _, _, _, _}} = jc:get(222),
    {error, not_found} = jc:get(333),
    {ok, {444, <<"444">>, t, s, _, _, _, _}} = jc:get(444),
    timer:sleep(1100),
    {error, not_found} = jc:get(222),
    {error, not_found} = jc:get(444),
    ok.

jc_scope_set(_Config) ->
    {ok, {keys, ["11", "1", "2", "3"]}} = 
	jc:mset([{"11","11"},{"1","1","T","S"},
		 {"2","2"},{"3","3","true", "true"}]),
    {ok, {keys, ["1", "3"]}} = 
	jc:type_set("new", [{"1", "1"}, {"3", "3", "true", "true"}, {"1.1"}]),
    {error, not_found} =  jc:type_get("true"),

    {ok, Lst} = jc:type_get("new"),    

    {"1", <<"1">>, "new", "undefined",_ ,_ , _, _} = lists:keyfind("1",1,Lst),
    {"3", <<"3">>, "new", "true",_ ,_ , _, _} = lists:keyfind("3",1,Lst),
    
    {ok, {keys, ["4", "3"]}} = jc:scope_set("undefined", [{"4","4"},{"3","3"}]),
    {ok,Lst2}= jc:scope_get("undefined"),

    {"4", <<"4">>, "undefined", "undefined",_ , _, _, _} = lists:keyfind("4",1,Lst2),
    {"3", <<"3">>, "undefined","undefined",_ , _, _, _} = lists:keyfind("3",1,Lst2),

    {error, not_found} =  jc:get("1"),
    ok.

set_scope_stash(_Config)->
    {ok, {keys, ["11", "1", "2", "3"]}} =
	jc:mset([{"11","11"},{"1","1","T","S"},
		 {"2","2"},{"3","3","true", "true"}]),
    {ok, {keys, ["1", "3"]}} = 
	jc:type_set("new", [{"1", "1"}, 
			    {"3", "3", "true", "true"}, 
			    {"1.1"}],"STASH"),
    {error, not_found} =  jc:type_get("true"),
    {error, not_found} = jc:type_get("STASH"),
    {ok,Lst} =  jc:type_get("new"),    
    {"3", <<"3">>, "new", "true",_ , _, _, _} = lists:keyfind("3",1,Lst),
    {"1", <<"1">>, "new", "undefined",_ , _, _, _}  = lists:keyfind("1",1,Lst),
    
    {ok, {keys, ["1"]}} = jc:type_set("new", [{"1", "1"}, {"1.1"}],"STASH"),
    {ok,[{"3", <<"3">>, "STASH", "true",_ , _, _, _}]} = 
	jc:type_get("STASH"),    

    {ok, {keys, ["4", "3"]}} = jc:scope_set("undefined", 
					    [{"4","4"},{"3","3"}],
					    "NEWSTASH"),
    {ok,Lst1} = jc:scope_get("undefined"),

    {"4", <<"4">>, "undefined", "undefined",_ , _, _, _} = lists:keyfind("4",1,Lst1),
    {"3", <<"3">>, "undefined","undefined",_ , _, _, _} = lists:keyfind("3",1,Lst1),

    {ok,Lst2} = 
	jc:scope_get("NEWSTASH"),

    {"2", <<"2">>, "undefined", "NEWSTASH",_ , _, _, _} = lists:keyfind("2",1,Lst2),
	 {"11",<<"11">>,"undefined","NEWSTASH",_ , _, _, _}  = lists:keyfind("11",1,Lst2),
	 {"1", <<"1">>, "new", "NEWSTASH",_ , _, _, _}  = lists:keyfind("1",1,Lst2),


    {error, badarg} = jc:scope_set("new", notlist),
    {error, badarg} = jc:scope_set("new", notlist, "trash"),
    {error, badarg} = jc:type_set("new", notlist),
    {error, badarg} = jc:type_set("new", notlist, "trash"),
    ok.


memcache(_Config)->
    {ok, {integer, 5}} = jc:append("New","hello"),
    {ok, {integer, 11}} = jc:append("New"," there"),
    {error, badarg} = jc:append("key", wont_work),

    {ok, {value, <<"hello there">>}} =  jc:get_value("New"),

    {ok, {integer, -1}} = jc:decr("Decr"),
    {ok, {integer, -1}} = jc:decr(decr),
    {ok, {integer, 0}} = jc:incr("Decr"),
    {ok, {integer, 0}} = jc:incr(decr),
    {ok, {integer, 10}} = jc:incrby("Decr", "10"),
    {ok, {integer, -10}} = jc:incrby("Decr", -20),
    {ok, {integer, 0}} = jc:decrby("Decr", "-10"),
    {ok, {integer, -5}} = jc:decrby("Decr", 5),
    {ok, {integer, -1}} = jc:incrby("Decr", "4"),
    {error, badarg} = jc:incrby("Decr", "h"),
    {error, badarg} = jc:incrby("Decr", atom),
    {error, badarg} = jc:decrby("Decr", "y"),
    {error, badarg} = jc:decrby("Decr", atom),
    

    {error, not_found} = jc:getset("Not","it"),
    {error, badarg} = jc:getset("Not", it),
    {ok, {key, "test"}} = jc:set("test", "a"),
    {ok, {value, <<"a">>}} = jc:getset("test", "b"),
    {ok, {value, <<"b">>}} = jc:getset("test", "c"),
    {ok, {value, <<"c">>}} = jc:get_value("test"),
    {ok, {key, "1"}} = jc:set("1", "2"),   
    {ok, {value, <<"2">>}} = jc:getset("1", "3"),   
    {ok, {value, <<"3">>}} = jc:get_value("1"),   
    {error, badarg} = jc:getset("1", {"11"}),
    {error, not_found}= jc:getset("2getset","1"),
    {error, value_not_integer} = jc:decr("test"),
    ok.

s_t_change_list(_Config) ->
    jc:flush(),

    jc:mset([{"11","11", t, s},{"22","22", t, s}]),
    jc:mset([{"33","33", t, s}, {"44","44", t, s}]),

    {ok, First} = jc:get("11"),
    {"11", <<"11">>, t, s, _C1, U1, _, _} = First,

    {ok, Second} = jc:get("22"),
    {"22", <<"22">>, t, s, _C2, _U2, _, _} = Second,

    {ok, Third} = jc:get("33"),
    {"33", <<"33">>, t, s, _C3, U3, _, _} = Third,

    {ok, Fourth} = jc:get("44"),
    {"44", <<"44">>, t, s, _C4, _U4, _, _} = Fourth,
    

    {error, badarg} = jc:minactivate(notlist),
    jc:minactivate(["22"]),
    jc:replace_value("33","333"),
    jc:set("55","55", t, s),

    {ok, Fifth} = jc:get("55"),
    {"55", <<"55">>, t, s, _C5, U5, _I5, _} = Fifth,
    
    {changelist, {A1, UU1, I1}} = jc:scope_change_list(s, U3),    
    true = lists:keymember("44", 1, A1),
    true = lists:keymember("55", 1, A1),
    2 = length(A1),
    true = lists:keymember("33", 1, UU1),
    true = lists:keymember("22", 1, I1),

    
    {changelist, {A2, UU2, I2}} = jc:type_change_list(t, U3),    
    true = lists:keymember("44", 1, A2),
    true = lists:keymember("55", 1, A2),
    2 = length(A2),
    true = lists:keymember("33", 1, UU2),
    true = lists:keymember("22", 1, I2),


    {changelist, {A3, UU3, I3}} = jc:change_list(U3),    
    true = lists:keymember("44", 1, A3),
    true = lists:keymember("55", 1, A3),
    2 = length(A3),
    true = lists:keymember("33", 1, UU3),
    true = lists:keymember("22", 1, I3),


    {changelists, [{t,{[{"44",<<"44">>,t,s,_,_,_,_},
			{"55",<<"55">>,t,s,_,_,_,_}],
		       [{"33",<<"333">>,t,s,_,_,_,_}],
		       [{"22",<<"22">>,t,s,_,_,_,_}]}},
		   {s,{[{"44",<<"44">>,t,s,_,_,_,_},
			{"55",<<"55">>,t,s,_,_,_,_}],
		       [{"33",<<"333">>,t,s,_,_,_,_}],
		       [{"22",<<"22">>,t,s,_,_,_,_}]}},
		   {eee,{[],[],[]}}]} = jc:change_lists(U3, [{type, t}, {scope,s}, 
					      {type, eee}, {cow, pig}]),




    {changelist, {A4, UU4, I4}} = jc:scope_change_list(s, 0),    
    true = lists:keymember("11", 1, A4),
    true = lists:keymember("33", 1, A4),
    true = lists:keymember("44", 1, A4),
    true = lists:keymember("55", 1, A4),
    [] = UU4,
    [] = I4,

    {changelist, {A5, UU5, I5}} = jc:type_change_list(t, 0),    
    true = lists:keymember("11", 1, A5),
    true = lists:keymember("33", 1, A5),
    true = lists:keymember("44", 1, A5),
    true = lists:keymember("55", 1, A5),
    [] = UU5,
    [] = I5,

    {changelist, {A6, U6, I6}} = jc:change_list(0),    
    true = lists:keymember("11", 1, A6),
    true = lists:keymember("33", 1, A6),
    true = lists:keymember("44", 1, A6),
    true = lists:keymember("55", 1, A6),
    [] = U6,
    [] = I6,


    {changelists, [{t,{[{"11",<<"11">>,t,s,_,_,_,_},
			{"44",<<"44">>,t,s,_,_,_,_},
			{"33",<<"333">>,t,s,_,_,_,_},
			{"55",<<"55">>,t,s,_,_,_,_}],
		       [],[]}},
		   {s,{[
			{"11",<<"11">>,t,s,_,_,_,_},
			{"44",<<"44">>,t,s,_,_,_,_},
			{"33",<<"333">>,t,s,_,_,_,_},
			{"55",<<"55">>,t,s,_,_,_,_}],
		       [],[]}},
		   {eee,{[],[],[]}}]} = jc:change_lists([{type, t}, {scope,s}, 
							{type, eee}, {cow, pig}]),





    {changelist, {A7, U7, I7}} = jc:scope_change_list(s, U1),    
    true = lists:keymember("33", 1, A7),
    true = lists:keymember("44", 1, A7),
    true = lists:keymember("55", 1, A7),
    [] = U7,
    [] = I7,

    {changelist, {A8, U8, I8}} = jc:type_change_list(t, U1),    
    true = lists:keymember("33", 1, A8),
    true = lists:keymember("44", 1, A8),
    true = lists:keymember("55", 1, A8),
    [] = U8,
    [] = I8,

    {changelist, {A9, U9, I9}} = jc:change_list(U1),    
    true = lists:keymember("33", 1, A9),
    true = lists:keymember("44", 1, A9),
    true = lists:keymember("55", 1, A9),
    [] = U9,
    [] = I9,

    {changelist, {[], [], []}} = jc:type_change_list(t, U5),
    {changelist, {[], [], []}} = jc:scope_change_list(s, U5),
    ok.

mset_inactivate(_Config) ->
    jc:flush(),

    jc:mset([{"11","11", t, s},{"22","22", t, s}]),
    jc:mset([{"33","33", t, s}, {"44","44", t, s}]),

    {ok, {"11", <<"11">>, t, s, _, _, undefined, _}} = 
	jc:get("11"),
    {ok, {"22", <<"22">>, t, s, _, _, undefined, _}} = 
	jc:get("22"),
    {ok, {"33", <<"33">>, t, s, _, _, undefined, _}} = 
	jc:get("33"),
    {ok, {"44", <<"44">>, t, s, _, _, undefined, _}} = 
	jc:get("44"),

    {error, badarg} = jc:mset_and_inactivate(notlist, 0, [], 0),
    {error, badarg} = jc:mset_and_inactivate(["11"], 0, notlist, 0),
    {error, badarg} = jc:mset_and_inactivate(["11"], notnumber, [], 0),
    {error, badarg} = jc:mset_and_inactivate(["11"], 0, [], -12),

    {ok, {keys, []}} = jc:mset_and_inactivate([], 0, ["11", "22"], 1),
    {ok, {"11", <<"11">>, t, s, _, _, IT1, _}} = jc:get("11"),
    {ok, {"22", <<"22">>, t, s, _,  _, IT2, _}} = jc:get("22"),
    true = is_number(IT1),
    true = (IT2 >= IT1),
    timer:sleep(1100),
    {error, not_found} = jc:get("11"),
    {error, not_found} = jc:get("22"),

    jc:mset_and_inactivate([{"11","11", t, s},
			    {"22","22", t, s},
			    {"33","33", t, s}, 
			    {"44","44", t, s}
			   ], 0, [], 0),

    {ok, {"11", <<"11">>, t, s, _, _, undefined, _}} = 
	jc:get("11"),
    {ok, {"22", <<"22">>, t, s, _, _, undefined, _}} = 
	jc:get("22"),
    {ok, {"33", <<"33">>, t, s, _, _, undefined, _}} = 
	jc:get("33"),
    {ok, {"44", <<"44">>, t, s, _, _, undefined, _}} = 
	jc:get("44"),
    

    jc:mset_and_inactivate([{"111","111", t, s},
			    {"222","222", t, s}
			   ],
			   1,
			   ["33","44"], 
			   2),

    {ok, {"111", <<"111">>, t, s, _, _, undefined, _}} = 
	jc:get("111"),
    {ok, {"22", <<"22">>, t, s, _, _, undefined, _}} = 
	jc:get("22"),
    {ok, {"222", <<"222">>, t, s, _, _, undefined, _}} = 
	jc:get("222"),
    {ok, {"33", <<"33">>, t, s, _, _, I1, _}} = 
	jc:get("33"),
    {ok, {"44", <<"44">>, t, s, _, _, I2, _}} = 
	jc:get("44"),
    
    true = (I2 >= I1),
    timer:sleep(1100),
    {error, not_found} = jc:get("111"),
    {error, not_found} = jc:get("222"),
    {ok, {"33", <<"33">>, t, s, _, _, I1, _}} = 
	jc:get("33"),
    timer:sleep(1100),
    {error, not_found} = jc:get("33"),
    {error, not_found} = jc:get("44"),
    ok.

concur_test(_Config)->
    insert_test(1),
    R0 = [spawn(?MODULE, concur_process, [self(), 50]) || _ <-lists:seq(1, 50)],
    [CPid ! start || CPid <- R0],
    [begin
	 receive 
	     {Pid, ok} ->
		 ok;
	     {Pid, Other} ->
		 ok = Other
	 end
     end || Pid <- R0],
    ok.

concur_process(Parent, N) ->
    Switch = crypto:rand_uniform(1,3),
    receive
	start ->
	    case Switch of
		1 -> 
		    io:format("Starting [~p] as an inserter~n",[self()]),
		    Parent ! {self(), insert_test(N)};
		2 -> 
		    io:format("Starting [~p] as a reader~n",[self()]),
		    Parent ! {self(), lookup_test(N)}
	    end
    end.

insert_test(0) -> 
    io:format("Inserter [~p] successful!~n",[self()]),
    ok;
insert_test(N) ->
    io:format("Writing [~p] 6 keys...~n",[self()]),
    X = crypto:rand_uniform(0,10),
    Y = crypto:rand_uniform(0,10-X),
    Z = 10 - (X+Y),

    {ok, {keys, _}} = jc:scope_set(not_used, 
				   [{a, lists:concat([X]), first, not_used},
				    {b, lists:concat([Y]), second, not_used},
				    {c, lists:concat([Z]), third, not_used}]),

    {ok, {keys, _}} = jc:mset([{d, lists:concat([X]), set1, not_used},
			       {e, lists:concat([Y]), set2, not_used},
			       {f, lists:concat([Z]), set3, not_used}]),
    insert_test(N-1).


lookup_test(0) -> 
    io:format("Reader [~p] successful!~n",[self()]),
    ok;
lookup_test(N) ->
    io:format("Reading [~p] 6 keys...~n",[self()]),
    {ok, R1} = jc:scope_get(not_used),

    S1 = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			      Val = list_to_integer(binary_to_list(V)),
			      Acc + Val end,
		      0,
		      R1),


    {ok, R2} = jc:mget([a,b,c]),
    S2 = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			       Val2 = list_to_integer(binary_to_list(V)),
			       Acc + Val2 end,
		       0,
		       R2),
    case S2 of
	10 when S1 == 10 -> lookup_test(N-1);
	10 when S1 == 20 -> lookup_test(N-1);
	_ -> 
	    io:format("S1 and S2 ~p ~p~nn",[S1, S2]),
	    false
    end.
