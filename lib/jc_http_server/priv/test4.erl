-module(test4).
-export([start/3, init/2, get/3]).


start(N, Limit, T) ->
    {Url, Port} = case T of
		      m ->
			  {"http://localhost:9000/change_list?type=p_index&since=0", 9000};
		      s ->
			  {"http://localhost:9000/change_list?since=0",9000}
    end,
    inets:start(),
    httpc:set_options([{proxy, {{"http://localhost", Port}, ["localhost"]}}]),
    R0 = [{X,spawn(?MODULE, init, [Limit, Url])} || X<-lists:seq(1, N)],
    io:format("spawned ~p processes.~n",[R0]),
    [C ! start || {_,C} <- R0].

init(Limit, Url) ->
    receive
	start ->
	    case timer:tc(fun() -> get(Limit, Limit, Url) end) of
		{Time, {ok, Goal}} ->
		    io:format("Get-Time(~p) completed ~p of ~p in ~p sec.~n",
			      [self(), Goal, Limit, Time/1000000]);
		{_Time, _Error} ->
		    io:format("Failed(~p)~n",[self()])
	    end,
	    init(0, Url);
	{http, {_RequestId, _Result}} ->
	    init(0, Url);
	Msg -> Msg
    end.



get(0, Goal, _Url) ->
  {ok, Goal};
		  
get(Limit, Goal, Url) ->
    httpc:request(get, {Url, []}, [], []),
    
    get(Limit-1, Goal, Url).


