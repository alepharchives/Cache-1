-module(concur).
-compile(export_all).


concur_test(_Config)->
    insert_test(1),
    R0 = [spawn(?MODULE, concur_process, [self(), 100]) || _ <-lists:seq(1, 10)],
    [CPid ! start || CPid <- R0],
    ok = results(10).


results(0) -> ok;
results(N) ->
    Response = receive 
		   {_Pid, Value}->
		       Value 
	       end,
    case Response of 
	error -> error;
	ok -> results(N-1)
    end.




concur_process(Parent, N) ->
    Switch = crypto:rand_uniform(1,2),
    receive
	start ->
	    case Switch of
		1 -> Parent ! {self(), insert_test(N)};
		2 -> Parent ! {self(), lookup_test(N)}
	    end
    end.

insert_test(0) -> ok;
insert_test(N) ->
    X = crypto:rand_uniform(0,10),
    Y = crypto:rand_uniform(0,10-X),
    Z = 10 - (X+Y),
    {ok, {keys, _}} = jc:scope_set(not_used, 
				   [{a, lists:concat([X]), first, not_used},
				    {b, lists:concat([Y]), second, not_used},
				    {c, lists:concat([Z]), third, not_used}]),

    {ok, {keys, _}} = jc:mset([{d, lists:concat([X]), first, not_used},
			       {e, lists:concat([Y]), second, not_used},
			       {f, lists:concat([Z]), third, not_used}]),
    insert_test(N-1).

lookup_test(0) -> ok;
lookup_test(N) ->
    {ok, Results} = jc:scope_get(not_used),
    {ok, Results2} = jc:mget([d,e,f]),
    Sum = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			      N = list_to_integer(binary_to_list(V)),
			      Acc + N end,
		      0,
		      Results),

    Sum2 = lists:foldl(fun({_,V,_,_, _,_,_,_}, Acc)->
			       N = list_to_integer(binary_to_list(V)),
			       Acc + N end,
		       0,
		       Results2),
    case Sum of
	Sum2 when Sum2 == 10-> lookup_test(N-1);
	false -> false
    end.
