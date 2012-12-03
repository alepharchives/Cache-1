-module(test_acid).
-export([start/2, insert/0,lookup_is_ten/0,init/2]).


start(N, Limit) ->
    _R0 = [{X, spawn(?MODULE, init, [Limit, insert])} || X<-lists:seq(1, N)],
    _R1 = [{X, spawn(?MODULE, init, [Limit, lookup])} || X<-lists:seq(1, N)],
    io:format("spawned ~p processes.~n",[N]).

init(Limit, Type) when Type == insert ->
	case timer:tc(fun() -> insert_loop(Limit, Limit) end) of
	    {Time, {ok, Goal}} ->
		io:format("Insert-Time(~p) completed ~p of ~p in ~p sec.~n",
			  [self(), Goal, Limit, Time/1000000]);
	    {_Time, _Error} ->
		io:format("Failed(~p)~n",[self()])
	end;

init(Limit, Type) when Type == lookup->
	case timer:tc(fun() -> lookup_loop(Limit, Limit) end) of
	    {Time, {ok, Goal}} ->
		io:format("Lookup-Time(~p) completed ~p of ~p in ~p sec.~n",
			  [self(), Goal, Limit, Time/1000000]);
	    {_Time, _Error} ->
		io:format("Failed(~p)~n",[self()])
	end.

insert_loop(0, Goal) ->
  {ok, Goal};
		  
insert_loop(Limit, Goal) ->
  case insert() of 
      {error, Reason} ->
	  {error, Reason};
      {aborted, Reason} ->
	  {aborted, Reason};
      _else ->
	  insert_loop(Limit-1, Goal)
end.

lookup_loop(0, Goal) ->
    {ok, Goal};
lookup_loop(Limit, Goal) ->
    case lookup_is_ten() of
	ok ->
	    lookup_loop(Limit -1 , Goal);
	Bad ->
	    io:format("lookup oops ---- ~p(~p)~n",[Bad, self()])
    end.



insert() ->
    X = crypto:rand_uniform(0,10),
    Y = crypto:rand_uniform(0,10-X),
    Z = 10 -(X+Y),
    case crypto:rand_uniform(0,2) of
	0 ->
	    A = fun()->
%			mnesia:wread({scope_to_keys,not_used}),
			B = fun() ->
				    jc_trx:set_scope_with(not_used,
				   [{a, lists:concat([X]), first, not_used},
				    {b, lists:concat([Y]), first, not_used},
				    {c, lists:concat([Z]), first, not_used}])
				    end,
			mnesia:async_dirty(B)
		end,
	    mnesia:async_dirty(A);
	1 ->
	    A = fun()->
%			mnesia:wread({scope_to_keys,not_used}),
			B = fun() ->
				    jc_trx:insert_keylist([{a, lists:concat([X]), 
					      first, not_used},
					     {b, lists:concat([Y]), 
					      first, not_used},
					     {c, lists:concat([Z]), 
					      first, not_used}],0,set)
			    end,
			mnesia:async_dirty(B)
		end,
	    mnesia:async_dirty(A)
    end.

lookup_is_ten()->
    Results = 
	case crypto:rand_uniform(0,2) of
	    0 -> {ok, R0} = jc:mget([a,b,c]), R0;
	    1 -> {ok, R1} = jc:scope_get(not_used), R1
	end,
    
    Sum = lists:foldl(fun({_,N,_,_,_,_,_,_},Acc)->
			      (Acc + list_to_integer(N)) end,
		      0,Results),
    case Sum of
	10 ->
	    ok;
	Oops ->
	    io:format("lookup oops ---- ~n",[]),
	    [Results| [bad, Oops]]
    end.
    
