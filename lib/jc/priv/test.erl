-module(test).
-export([start/3, init/2, get/3]).


start(N, Limit, T) ->
    R0 = [{X,spawn(?MODULE, init, [Limit, T])} || X<-lists:seq(1, N)],
    io:format("spawned ~p processes.~n",[R0]),
    [C ! start || {_,C} <- R0].

init(Limit, T) ->
    receive
	start ->
	    case timer:tc(fun() -> get(Limit, Limit, T) end) of
		{Time, {ok, Goal}} ->
		    io:format("Get-Time(~p) completed ~p of ~p in ~p sec.~n",
			      [self(), Goal, Limit, Time/1000000]);
		{_Time, _Error} ->
		    io:format("Failed(~p)~n",[self()])
	    end,
	    init(0, T);
	{http, {_RequestId, _Result}} ->
	    init(0, T);
	Msg -> Msg
    end.



get(0, Goal, _T) ->
  {ok, Goal};


get(Limit, Goal, http) ->
    Url = "http://localhost:9000/change_lists?scope=10008&scope=UnitQueue",
    {ok, {{_HttpVer, _Code, _Msg}, _Headers, _Body}} =
	httpc:request(get, {Url, [{"Accept-Encoding","gzip"}]}, [],[]),
    get(Limit-1, Goal, http);

		  
get(Limit, Goal, _) ->
    jc:change_lists([{scope, "10008"},{scope, "UnitQueue"}]),
    get(Limit-1, Goal, p).

	


