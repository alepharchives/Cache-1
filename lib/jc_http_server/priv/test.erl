-module(test).
-export([start/2, loop/1, init/1, load/1]).

start(N, Limit)->
    Result = [{X, spawn(?MODULE, init, [Limit])} || X<-lists:seq(1, N)],
    io:format("spawned ~p processes.~n",[N]),
    Result.


init(Limit) ->
    {Time, ok} = timer:tc(fun() ->loop(Limit) end),
    io:format("Time(~p) completed ~p in ~p sec.~n",
			  [self(), Limit, Time/1000000]).

loop(0) ->
    ok;
loop(Limit) ->
    Lookup = lists:concat([crypto:rand_uniform(2000)]),
    UA = "cl_jc test agent",
%    Url = "http://localhost:9000/read?key=" ++ Lookup,
    Url = "http://107.22.186.201/read?key=" ++ Lookup,
    {ok, {{_HttpVer, _Code, _Msg}, _Headers, _Body}} =
	http:request(get, {Url, [{"User-Agent", UA}]}, []),

    loop(Limit -1).

load(0) ->
    ok;
load(N) ->
    jc:insert(list_to_binary(lists:concat([N])), 
			"<FORM method = 'post' action='http://localhost:9000/write'>,<input type = 'text' name = 'item' size = 80 value = '{'key':'bed112', 'value':{'bed113':{'id':'1213','status':'occupied'}},'type':'bed','scope':'3A'}' /><br /> <P><INPUT type='submit' value='Submit'></P>  </FORM>",


			list_to_binary(lists:concat([N div 10])),
			list_to_binary(lists:concat([N div 5]))),

    
load(N-1).
