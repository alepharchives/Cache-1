%%% @author  Jim Rosenblum
%%% @copyright (C) 2012, 
%%% @doc Useful Utility Functions
%%% @end
%%% Created : 12 Aug 2012 by  Jim Rosenblulm

-module(jc_util).


-export([now_to_Uepoch/0, 
	 date_time_to_string_long/1,
	 date_to_string/1,
	 date_to_string_long/1,
	 get_env/3]).

%-------------------------------------------------------------------------------
% @doc
% Return the number of micro-seconds 1/1/1970 -- UNIX-style epoch
% @end
%-------------------------------------------------------------------------------
-spec now_to_Uepoch() -> pos_integer().

now_to_Uepoch() ->
    {A,B,C} = now(),
    ((A*1000000+B)*1000000) + C.


-spec get_env(atom(), atom(), any()) -> any().

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.


date_time_to_string_long(null) -> "undef";
date_time_to_string_long({{SY,SM,SD},{SH,SI,SS}}) ->
    lists:flatten([httpd_util:month(SM), " ", io_lib:format("~2..0B",[SD]), ", " , 
		  io_lib:format("~4..0B",[SY]), " ",
		  io_lib:format("~2..0B",[SH]), ":", io_lib:format("~2..0B",[SI]), ":", 
		  io_lib:format("~2..0B",[SS])]).


date_to_string(null) -> "undef";
date_to_string({{SY,SM,SD},_Time}) ->
   lists:flatten( [io_lib:format("~2..0B", [SM]), "/", 
		  io_lib:format("~2..0B",[SD]), "/", 
		  io_lib:format("~4..0B",[SY])]).

date_to_string_long(null) -> "undef";
date_to_string_long({{SY,SM,SD},_Time}) ->
    lists:append([httpd_util:month(SM), " ", lists:concat([SD]), ", " , lists:concat([SY])]).
