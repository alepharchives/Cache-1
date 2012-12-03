%%%-------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc
%%% Common test suite for jc_store
%%% @end
%%% Created : 29 Sep 2012 by  <Jim Rosenblum>
%%%-------------------------------------------------------------------
-module(change_relay_SUITE).

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
    ok = application:start(sasl),
    lager:start(),
    lager:set_loglevel(lager_console_backend, error),
    ok = application:start(resource_discovery),
    ok = application:start(jc),
    ok = application:start(change_relay),
    error_logger:tty(false),

    %pull out the tid of the ets tables used by change_elay
    {status, _Pid, _Mod, [_PDic, _SysState, _Parent, _Dbg, Misc]} =
	sys:get_status(change_relay),
    DataList = proplists:get_all_values(data, Misc),
    [Props] = [[{sub_table, Sub}, 
		{cli_table, Cli}, 
		{buf_table, Buf}, 
		{clean_buffer_ms, ET},
		{evict_deadbeats_ms, DB}] || 
		  [{"State",{state, Sub, Cli, Buf, ET, DB}}] <-DataList],
    Props ++ Config.

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
    [{cr, 
      [], 
      [start, subscribe]}
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
    [{group, cr}].

 
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
start(Config) ->
    0 = ets:info(?config(sub_table, Config), size),
    0 = ets:info(?config(cli_table, Config), size),
    0 = ets:info(?config(buf_table, Config), size),
    0 = change_relay:client_count(),
    {ok, {integer, 0}} = change_relay:load(),
    ok.

subscribe(Config) ->
    ok = change_relay:subscribe(self(), key, 1),
    Pid = self(),
    1 = ets:info(?config(cli_table, Config), size),
    ([{Pid, CliRef}]) = ets:lookup(?config(cli_table, Config), self()),
    [{{key, 1}, S}] = ets:lookup(?config(sub_table, Config), {key, 1}),
    sets:is_element(self(), S),

    ok = change_relay:subscribe(self(), type, type_test),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{type, type_test}, S2}] = ets:lookup(?config(sub_table, Config), 
					   {type, type_test}),
    sets:is_element(self(), S2),

    ok = change_relay:subscribe(self(), scope, scope_test),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{scope, scope_test}, S3}] = ets:lookup(?config(sub_table, Config), 
					     {scope, scope_test}),
    sets:is_element(self(), S3),

    ok = change_relay:subscribe(self(), {type, records}, type_test),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{{type, records}, type_test}, S4}] = ets:lookup(?config(sub_table, Config), 
						      {{type, records}, type_test}),
    sets:is_element(self(), S4),

    ok = change_relay:subscribe(self(), {scope, records}, scope_test),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{{scope, records}, scope_test}, S5}] = ets:lookup(?config(sub_table, Config), 
							{{scope, records}, scope_test}),
    sets:is_element(self(), S5),

    ok = change_relay:subscribe(self(), {key, records}, 1),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{{key, records}, 1}, S6}] = ets:lookup(?config(sub_table, Config), 
							{{key, records}, 1}),
    sets:is_element(self(), S6),

    ok = change_relay:subscribe(self(), all, records),
    1 = ets:info(?config(cli_table, Config), size),
    [{Pid, CliRef}] = ets:lookup(?config(cli_table, Config), self()),
    [{{all, records}, S7}] = ets:lookup(?config(sub_table, Config), 
							{all, records}),
    sets:is_element(self(), S7),

    ok = change_relay:unsubscribe(Pid, {key, records}, 1),
    1 = ets:info(?config(cli_table, Config), size),
    [] = ets:lookup(?config(sub_table, Config), {{key, records}, 1}),

    ok = change_relay:unsubscribe(Pid, scope,  all),
    ok = change_relay:unsubscribe(Pid, type,  all),
    ok = change_relay:unsubscribe(Pid, key,  all),
    ok = change_relay:unsubscribe(Pid, {type, records}, all),
    ok = change_relay:unsubscribe(Pid, {scope, records}, all),
    ok = change_relay:unsubscribe(Pid, all, records),

    0 = ets:info(?config(sub_table, Config), size),
    0 = ets:info(?config(buf_table, Config), size),
    ok.

