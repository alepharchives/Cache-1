-module(concur_test).
-compile(export_all).


test(Ps, Ts)->
    insert_test(1),
    R0 = [spawn(?MODULE, concur_process, [self(), Ts]) || _ <-lists:seq(1, Ps)],
    [CPid ! start || CPid <- R0],
    [begin
	 receive 
	     {Pid, ok} ->
		 ok;
	     {Pid, Other} ->
		 io:format("!!!!!!!!!!!!!!!!BAD ~p~n",[ Other])
	 end
     end || Pid <- R0],
    ok.

concur_process(Parent, N) ->
    Switch = crypto:rand_uniform(1,3),
    receive
	start ->
	    case Switch of
		1 -> 
		    io:format("Starting [~p] as an writer~n",[self()]),
		    Parent ! {self(), insert_test(N)};
		2 -> 
		    io:format("Starting [~p] as a reader~n",[self()]),
		    Parent ! {self(), lookup_test(N)}
	    end
    end.

insert_test(0) -> 
    io:format("Writer [~p] successful!~n",[self()]),
    ok;
insert_test(N) ->
    X = crypto:rand_uniform(0,10),
    Y = crypto:rand_uniform(0,10-X),
    Z = 10 - (X+Y),

    {ok, {keys, _}} = jc:scope_set(not_used, 
				   [{a, lists:concat([X]), first, not_used},
				    {b, lists:concat([Y]), second, not_used},
				    {c, lists:concat([Z]), third, not_used}]),

    {ok, {keys, _}} = jc:mset([{d, lists:concat([X]), set2, not_used},
			       {e, lists:concat([Y]), set2, not_used},
			       {f, lists:concat([Z]), set2, not_used}]),
    insert_test(N-1).

lookup_test(0,false)->
    false.

lookup_test(0) -> 
    io:format("Reader [~p] successful!~n",[self()]),
    ok;
lookup_test(N) ->
    {ok, R0} = case catch jc:type_get(set2) of
			{ok, Something} -> {ok, Something};
			_Oops -> {ok, [{10,<<"20">>,10,10,10,10,10,10}]}
		    end,

    S0 = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			      Val = list_to_integer(binary_to_list(V)),
			      Acc + Val end,
		      0,
		     R0),

    {ok, R1} = jc:scope_get(not_used),

    S1 = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			      Val = list_to_integer(binary_to_list(V)),
			      Acc + Val end,
		      0,
		      R1),

    {ok, R2} = jc:mget([a,b,c]),

    S2 = lists:foldl(fun({_,V,_,_,_,_,_,_}, Acc)->
			       Val2 = list_to_integer(binary_to_list(V)),
			       Acc + Val2 end,
		       0,
		       R2),
    case S2 of
	10 when S1 == 10, S0 == 10;
		S1 == 10, S0 == 20-> lookup_test(N-1);
	10 when S1 == 20, S0 == 10;
		S1 == 20; S0 == 20-> lookup_test(N-1);
	_ -> 
	    io:format("S1 and S2 ~p ~p~n",[S1, S2]),
	    lookup_test(0, false)
    end.
