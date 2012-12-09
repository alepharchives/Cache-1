%%%-------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc
%%% Common test suite for jc_http_server
%%% @end
%%% Created : 29 Sep 2012 by  <Jim Rosenblum>
%%%-------------------------------------------------------------------
-module(jc_http_SUITE).

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
    [{timetrap,{seconds,30*60}}].

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
    application:start(change_relay),
    application:start(jc_http_server),
    inets:start(),
    error_logger:tty(false),

    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    jc:flush(),
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
    [{http, 
      [], 
      [add, madd, append, incr, decr, getset, replace, mreplace, replace_vals,
      set, mset, scope_set, scope_set_stash, type_set, type_set_stash, delete,
      meta]}
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
    [{group, http}].

 
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
add(_Config) ->
    UrlResponse = "localhost:3000/key/bed112",
    Url = "http://localhost:3000/add",

    Item = "item={\"key\":\"bed112\",\"value\":\"{\\\"bed112\\\":{\\\"id\\\":\\\"1213\\\",\\\"status\\\":\\\"occupied\\\"}}\",\"type\":\"bed\",\"scope\":\"3A\"}",
    
    {ok,{{"HTTP/1.1",201,"Created"},_, Return}} = httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"operation","add"},
	      {"urls", {array,[{array,[UrlResponse]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {"http://"++UrlResponse, []}, [], []),
    
   
    {struct, [{"time",_}, 
	      {"elements", {array, [{struct, [{"bed112", 
					       {struct,
						[{"value",
						  {struct,
						   [{"bed112",
						     {struct,
						      [{"id","1213"},
						       {"status",
							"occupied"}]}}]}},
						 {"type","bed"},
						 {"scope","3A"},
						 {"create_tm",_},
						 {"last_update",_},
						 {"inactive","undefined"}]}}]}]}}]} = 
	mochijson:decode(Get),

    ok.

madd(_Config) ->
% Writing three keys in one trx, bed112 should fail, json return
    UrlResp1 = "localhost:3000/key/bed113",
    UrlResp2 = "localhost:3000/key/bed114",
    Url = "http://localhost:3000/add",

    Item = "item={\"key\":\"bed112\",\"value\":\"{\\\"bed112\\\":{\\\"id\\\":\\\"1213\\\",\\\"status\\\":\\\"occupied\\\"}}\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed113\",\"value\":\"{\\\"bed113\\\":{\\\"id\\\":\\\"1213\\\",\\\"status\\\":\\\"occupied\\\"}}\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed114\",\"value\":\"{\\\"bed114\\\":{\\\"id\\\":\\\"1214\\\",\\\"status\\\":\\\"occupied\\\"}}\",\"type\":\"bed\",\"scope\":\"3A\"}",

    
    {ok,{{"HTTP/1.1",201,"Created"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"operation","add"},
	      {"urls", {array,[{array,[UrlResp1, UrlResp2]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get1}} = httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,
     [{"time",_},
      {"elements",
       {array,
	[{struct,
	  [{"bed113",
	    {struct,
	     [{"value",
	       {struct,
		[{"bed113",
		  {struct,
		   [{"id","1213"},{"status","occupied"}]}}]}},
	      {"type","bed"},
	      {"scope","3A"},
	      {"create_tm",_},
	      {"last_update",_},
	      {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get1),

    {ok,{{"HTTP/1.1",200,"OK"}, _, Get2}} = httpc:request(get, {"http://"++UrlResp2, []}, [], []),
    
   
    {struct,
     [{"time",_},
      {"elements",
       {array,
	[{struct,
	  [{"bed114",
	    {struct,
	     [{"value",
	       {struct,
		[{"bed114",
		  {struct,
		   [{"id","1214"},{"status","occupied"}]}}]}},
	      {"type","bed"},
	      {"scope","3A"},
	      {"create_tm",_},
	      {"last_update",_},
	      {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get2),

    ok.


append(_Config) ->
% Append method
    UrlResp1 = "localhost:3000/key/Append",
    Url = "http://localhost:3000/append",

    Item = "item={\"key\":\"Append\", \"value\":1,\"type\":\"bed\",\"scope\":\"3A\"}",
    


    {ok,{{"HTTP/1.1",200,"OK"},
	 _, 
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"integer",{struct,[{"value",1}]}},
	      {"urls", {array,[{array,[UrlResp1]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,[{"time", _},
	     {"elements",
	      {array,[{struct,[{"Append",
				{struct,[{"value",1},
					 {"type","undefined"},
					 {"scope","undefined"},
					 {"create_tm",_},
					 {"last_update",_},
					 {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get),
ok.

incr(_Config) ->
% Incr method
    UrlResp1 = "localhost:3000/key/Numeric",
    Url = "http://localhost:3000/increment",

    Item = "item={\"key\":\"Numeric\", \"value\":10}",
    
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _, 
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"integer", {struct,[{"value",10}]}},
	      {"urls", {array,[{array,[UrlResp1]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,[{"time",_},
         {"elements",
          {array,[{struct,[{"Numeric",
                            {struct,[{"value",10},
                                     {"type","undefined"},
                                     {"scope","undefined"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get),
ok.


decr(_Config) ->
% Decr method
    UrlResp1 = "localhost:3000/key/Numeric",
    Url = "http://localhost:3000/decrement",

    Item = "item={\"key\":\"Numeric\", \"value\":10}",
    
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"integer", {struct,[{"value",0}]}},
	      {"urls", {array,[{array,[UrlResp1]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,[{"time",_},
         {"elements",
          {array,[{struct,[{"Numeric",
                            {struct,[{"value",0},
                                     {"type","undefined"},
                                     {"scope","undefined"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get),
ok.

getset(_Config) ->

    UrlResp1 = "localhost:3000/key/Numeric",
    Url = "http://localhost:3000/getset",

    Item = "item={\"key\":\"Numeric\", \"value\":43}",
    
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"field",{struct,[{"value",0}]}},
	      {"urls", {array,[{array,[UrlResp1]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,[{"time",_},
         {"elements",
          {array,[{struct,[{"Numeric",
                            {struct,[{"value",43},
                                     {"type","undefined"},
                                     {"scope","undefined"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get),
ok.


replace(_Config) ->

    UrlResp1 = "localhost:3000/key/bed112",
    Url = "http://localhost:3000/replace",

    Item = "item={\"key\":\"bed112\", \"value\":\"\\\"replaced value\\\"\",\"type\":0,\"scope\":\"3A\"}",
    
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
   {struct,[{"time", _},
	    {"operation","replace"},
	    {"urls",{array,[{array,[UrlResp1]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://"++UrlResp1, []}, [], []),
    
   
    {struct,[{"time", _},
	     {"elements",
	      {array,[{struct,[{"bed112",
				{struct,[{"value","replaced value"},
					 {"type",0},
					 {"scope","3A"},
					 {"create_tm",_},
                                     {"last_update",_},
					 {"inactive","undefined"}]}}]}]}}]}= mochijson:decode(Get),
ok.

mreplace(_Config) ->
% Replace three keys at once, bed115 should fail, one has ttl
    UrlResp1 = "localhost:3000/key/bed112",
    UrlResp2 = "localhost:3000/key/bed113",
    Url = "http://localhost:3000/replace",

    Item = "item={\"key\":\"bed112\", \"value\":\"\\\"should have this new value\\\"\",\"type\":12.34,\"scope\":\"3A\",\"ttl\":1}&item={\"key\":\"bed113\", \"value\":null,\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed115\", \"value\":\"\\\"should fail\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}",
    
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),

    {struct,[{"time",_},
	     {"operation","replace"},
	     {"urls",
	      {array,[{array,[UrlResp1, UrlResp2]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, {"http://localhost:3000/keys/bed112?key=bed113", []}, [], []),

    {struct,[{"time",_},
         {"elements",
          {array,[{struct,[{"bed112",
                            {struct,[{"value","should have this new value"},
                                     {"type", 12.34},
                                     {"scope","3A"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]},
                  {struct,[{"bed113",
                            {struct,[{"value",null},
                                     {"type","bed"},
                                     {"scope","3A"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]}]}}]} = mochijson:decode(Get),

    timer:sleep(2000),
    {error, not_found} = jc:get("bed112"),

    ok.

replace_vals(_Config)->
    Url = "http://localhost:3000/",

    ItemS = "item={\"key\":\"bed113\", \"value\":\"3A-replace\"}",
    ItemT = "item={\"key\":\"bed113\", \"value\":\"bed-replace\"}",
    ItemV = "item={\"key\":\"bed113\", \"value\":\"\\\"value-replace\\\"\"}",

    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 _Return}} = 
	httpc:request(put, {Url ++ "replace_scope", [], "application/x-www-form-urlencoded", ItemS}, 
		      [], []),    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 _Return2}} = 
	httpc:request(put, {Url ++ "replace_type", [], "application/x-www-form-urlencoded", ItemT}, 
		      [], []),    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _,
	 _Return3}} = 
	httpc:request(put, {Url ++ "replace_value", [], "application/x-www-form-urlencoded", ItemV}, 
		      [], []),    

    {ok,{{"HTTP/1.1",200,"OK"},_, GetS}} = 
	httpc:request(get, {"http://localhost:3000/key/scope/bed113", []}, [], []),


    {ok,{{"HTTP/1.1",200,"OK"},_, GetT}} = 
	httpc:request(get, {"http://localhost:3000/key/type/bed113", []}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, GetV}} = 
	httpc:request(get, {"http://localhost:3000/key/value/bed113", []}, [], []),
    

    {struct,[{"time",_},
	     {"field",{struct,[{"scope","3A-replace"}]}}]} = mochijson:decode(GetS),

    {struct,[{"time",_},
	     {"field",{struct,[{"type","bed-replace"}]}}]} = mochijson:decode(GetT),

    {struct,[{"time",_},
	     {"field",{struct,[{"value","value-replace"}]}}]} = mochijson:decode(GetV),
    

    ok.

    
set(_Config) ->
    UrlResp = "localhost:3000/key/bed112",
    Url = "http://localhost:3000/set",

    Item = "item={\"key\":\"bed112\", \"value\":\"\\\"I set this\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}",
    


    {ok,{{"HTTP/1.1",201,"Created"},
	 _,
	 Return}} = httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"operation","set"},
	      {"urls", {array,[{array,[UrlResp]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {"http://"++UrlResp, []}, [], []),
    
   
    {struct, [{"time",_}, 
	      {"elements", {array, [{struct, [{"bed112", 
					       {struct,
						[{"value","I set this"},
						 {"type","bed"},
						 {"scope","3A"},
						 {"create_tm",_},
						 {"last_update",_},
						 {"inactive","undefined"}]}}]}]}}]} = 
	mochijson:decode(Get),

    ok.


mset(_Config) ->
% Set Method: Writing multiple keys in one ACID transaction
    UrlResp1 = "localhost:3000/key/bed116",
    UrlResp2 = "localhost:3000/key/bed117",
    UrlResp3 = "localhost:3000/key/bed118",
    UrlResp4 = "localhost:3000/key/bed119",
    Url = "http://localhost:3000/set",

    Item = "item={\"key\":\"bed116\", \"value\":\"\\\"new value\\\"\"}&item={\"key\":\"bed117\",\"value\":\"\\\"new value\\\"\",\"ttl\":1}&item={\"key\":\"bed118\",\"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed119\",\"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\", \"ttl\":1}",
    

    
    {ok,{{"HTTP/1.1",201,"Created"},
	 _,
	 Return}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),
    
    {struct, [{"time",_},
	      {"operation","set"},
	      {"urls", {array,[{array,[UrlResp1, UrlResp2, UrlResp3, UrlResp4]}]}}]} = mochijson:decode(Return),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = 
	httpc:request(get, 
		      {"http://localhost:3000/keys/bed116?key=bed117&key=bed118&key=bed119",
		       []}, [], []),
    
    {struct,[{"time",_},
         {"elements",
          {array,[{struct,[{"bed116",
                            {struct,[{"value","new value"},
                                     {"type","undefined"},
                                     {"scope","undefined"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]},
                  {struct,[{"bed117",
                            {struct,[{"value","new value"},
                                     {"type","undefined"},
                                     {"scope","undefined"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]},
                  {struct,[{"bed118",
                            {struct,[{"value","a value"},
                                     {"type","bed"},
                                     {"scope","3A"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]},
                  {struct,[{"bed119",
                            {struct,[{"value","a value"},
                                     {"type","bed"},
                                     {"scope","3A"},
                                     {"create_tm",_},
                                     {"last_update",_},
                                     {"inactive","undefined"}]}}]}]}}]}= mochijson:decode(Get),

    timer:sleep(1500),
    {error, not_found} = jc:get("bed117"),
    {error, not_found} = jc:get("bed119"),

ok.

scope_set(_Config) ->
    Url = "http://localhost:3000/scope_set",
    UrlResp = "http://localhost:3000/scope/3A",
    Item ="target=3A&item={\"key\":\"bed119\", \"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\",\"ttl\":1}&item={\"key\":\"bed120\", \"value\":\"\\\"bed120s value\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed121\", \"value\":\"\\\"a value\\\"\"}",
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _, _}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {UrlResp, []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed119",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed121",
		 {struct,
		  [{"value","a value"},
		   {"type","undefined"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get),
    timer:sleep(1500),
    {error, not_found} = jc:get("bed119"),
    ok.

scope_set_stash(_Config)->
    Url = "http://localhost:3000/scope_set",
    UrlResp = "http://localhost:3000/scope/3A",
    Item ="target=3A&stash=STASH&item={\"key\":\"bed1119\", \"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\",\"ttl\":1}&item={\"key\":\"bed1120\", \"value\":\"\\\"bed120s value\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed1121\", \"value\":\"\\\"a value\\\"\"}",
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _, _}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {UrlResp, []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed1119",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed1120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed1121",
		 {struct,
		  [{"value","a value"},
		   {"type","undefined"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get),
    timer:sleep(1500),
    {error, not_found} = jc:get("bed1119"),
    {ok,{{"HTTP/1.1",200,"OK"},_, Get2}} = httpc:request(get, {"http://localhost:3000/scope/STASH", []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed121",
		 {struct,
		  [{"value","a value"},
		   {"type","undefined"},
		   {"scope","STASH"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","bed"},
		   {"scope","STASH"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get2),
    ok.




type_set(_Config) ->
    Url = "http://localhost:3000/type_set",
    UrlResp = "http://localhost:3000/type/bed",
    Item ="target=bed&item={\"key\":\"bed119\", \"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\",\"ttl\":1}&item={\"key\":\"bed120\", \"value\":\"\\\"bed120s value\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed121\", \"value\":\"\\\"a value\\\"\"}",
    
    {ok,{{"HTTP/1.1",200,"OK"},_, _}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {UrlResp, []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed119",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed121",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","undefined"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get),
    timer:sleep(1500),
    {error, not_found} = jc:get("bed119"),
    ok.

type_set_stash(_Config)->
    Url = "http://localhost:3000/type_set",
    UrlResp = "http://localhost:3000/type/bed",
    Item ="target=bed&stash=STASHTYPE&item={\"key\":\"bed1119\", \"value\":\"\\\"a value\\\"\",\"type\":\"bed\",\"scope\":\"3A\",\"ttl\":1}&item={\"key\":\"bed1120\", \"value\":\"\\\"bed120s value\\\"\",\"type\":\"bed\",\"scope\":\"3A\"}&item={\"key\":\"bed1121\", \"value\":\"\\\"a value\\\"\"}",
    
    {ok,{{"HTTP/1.1",200,"OK"},
	 _, _}} = 
	httpc:request(put, {Url, [], "application/x-www-form-urlencoded", Item}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Get}} = httpc:request(get, {UrlResp, []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed1119",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed1120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","bed"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed1121",
		 {struct,
		  [{"value","a value"},
		   {"type","bed"},
		   {"scope","undefined"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get),
    timer:sleep(1500),
    {error, not_found} = jc:get("bed1119"),
    {ok,{{"HTTP/1.1",200,"OK"},_, Get2}} = httpc:request(get, {"http://localhost:3000/type/STASHTYPE", []}, [], []),
     
    {struct,
     [{"time",_},
      {"changeList",
       {array,
	[{struct,
	  [{"removedData",{array,[]}},
	   {"addedData",
	    {array,
	     [{struct,
	       [{"bed121",
		 {struct,
		  [{"value","a value"},
		   {"type","STASHTYPE"},
		   {"scope","undefined"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]},
	      {struct,
	       [{"bed120",
		 {struct,
		  [{"value","bed120s value"},
		   {"type","STASHTYPE"},
		   {"scope","3A"},
		   {"create_tm",_},
		   {"last_update",_},
		   {"inactive","undefined"}]}}]}]}},
	   {"changedData",{array,[]}}]}]}}]} = mochijson:decode(Get2),
    ok.

delete(_Config) ->
    {ok, _} = jc:get("bed120"),
    {ok,{{"HTTP/1.1",200,"OK"},_, _}} = 
	httpc:request(delete, {"http://localhost:3000/delete/key/bed120", []}, [], []),
    {error, not_found} = jc:get("bed120"),

    {ok, _} = jc:get("bed1120"),
    {ok, _} = jc:get("bed1121"),
    {ok,{{"HTTP/1.1",200,"OK"},_, _}} = 
	httpc:request(delete, {"http://localhost:3000/delete/keys/bed1120?key=bed1121", []}, [], []),
    {error, not_found} = jc:get("bed1120"),
    {error, not_found} = jc:get("bed1121"),

    {ok, _} = jc:scope_get("undefined"),
    {ok,{{"HTTP/1.1",200,"OK"}, _, _}} = 
	httpc:request(delete, {"http://localhost:3000/delete/scope/undefined", []}, [], []),
    {error, not_found} = jc:scope_get("undefined"),


    jc:madd([{1,"1","STASHTYPE","scope"},
	     {2,"2","STASHTYPE","scope"},
	     {3,"3","other","scope"},
	     {4,"4","other","scope"}]),

    {ok, _} = jc:type_get("STASHTYPE"),
    {ok,{{"HTTP/1.1",200,"OK"},_, _}} = 
	httpc:request(delete, {"http://localhost:3000/delete/type/STASHTYPE", []}, [], []),
    {error, not_found} = jc:type_get("STASHTYPE"),
    {ok, _} = jc:scope_get("scope"),


    {ok,{{"HTTP/1.1",200,"OK"},_, _}} = 
	httpc:request(delete, {"http://localhost:3000/flush", []}, [], []),
    {size, [{stats, _, _},
	    {ttl, 0, _},
	    {key_to_value, 0, _},
	    {schema, _,_}]}= jc:cache_size(),

    ok.

meta(_Config) ->
    {ok,{{"HTTP/1.1",200,"OK"},_, Quip}} = 
	httpc:request(get, {"http://localhost:3000/", []}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Size}} = 
	httpc:request(get, {"http://localhost:3000/size", []}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Resources}} = 
	httpc:request(get, {"http://localhost:3000/resources", []}, [], []),

    {ok,{{"HTTP/1.1",200,"OK"},_, Up}} = 
	httpc:request(get, {"http://localhost:3000/up", []}, [], []),

    {struct,[{"time",_},
	     {"relax","...JCache is up"}]} = mochijson:decode(Quip),
    
    {struct,
     [{"time",_},
      {"sizes",
       {array,
	[{struct,
	  [{"stats",
	    {struct,[{"records",_},{"words",_}]}}]},
	 {struct,
	  [{"ttl",
	    {struct,[{"records",_},{"words",_}]}}]},
	 {struct,
	  [{"key_to_value",
	    {struct,[{"records",_},{"words",_}]}}]},
	 {struct,
	  [{"schema",
	    {struct,
	     [{"records",_},{"words",_}]}}]}]}}]} = mochijson:decode(Size),

    {struct, [{"time", _}, 
	      {"cache_resources", {array, _ResourceList}}]} = mochijson:decode(Resources),

    {struct,
     [{"time",_},
      {"stats",
       {array,
	[{struct,
	  [{"up_at",_}]},
	 {struct,[{"now",_}]},
	 {struct,
	  [{"up_time",
	    {array,
	     [{struct,[{"days",_}]},
	      {struct,[{"hours",_}]},
	      {struct,[{"minutes",_}]},
	      {struct,[{"seconds",_}]}]}}]}]}}]}= mochijson:decode(Up),
    
    ok.




