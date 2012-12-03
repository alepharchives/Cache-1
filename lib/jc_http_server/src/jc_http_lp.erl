%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011-2012, Jim Rosenblum
%%% @doc This module handles the longpolling on behalf of jc_http_sever. The 
%%% functions in this module send a chunked header to the http client, then 
%%% subscribe to JCache changes of the type representing the long polling
%%% request. Once those changes are detected, they are passed on to the http client.
%%% @end
%%% Created : 26 Oct 2012 by Jim Rosenblum
%%%-----------------------------------------------------------------------------

-module(jc_http_lp).


-export([init_key_lp/2,
	 init_scope_lp/3,
	 init_type_lp/3,
	 init_change_list_lp/2]).

-include("../../include/records.hrl").
-include("../../include/types.hrl").


-define(RESUBSCRIBE, "{\"info\":\"longpoll_timeout\"}").
-define(LONGPOLL_TIME_OUT_MSEC, 1000*60*5). % 5 minutes

%%==============================================================================
%% Functions to faciliate longpolling
%%==============================================================================

%-------------------------------------------------------------------------------
% Subscribe to cache updates, recover if subscription service goes down and 
% clean-up after the long polling session ends (update received and sent)
%-------------------------------------------------------------------------------
init_key_lp(Req, Keys)->
    Ref = erlang:monitor(process, change_relay),
    [change_relay:subscribe(self(), {key, records}, Key) || Key <- Keys],

    start_lp(Req, keys, void, void),

    erlang:demonitor(Ref),
    change_relay:unsubscribe(self(),{key, records}, all).

init_scope_lp(Req, Scope, Since)->
    Ref = erlang:monitor(process, change_relay),
    change_relay:subscribe(self(), {scope, records}, Scope),

    start_lp(Req, scope, Since, Scope),

    erlang:demonitor(Ref),
    change_relay:unsubscribe(self(), {scope, records}, all).

init_type_lp(Req, Type, Since)->
    Ref = erlang:monitor(process, change_relay),
    change_relay:subscribe(self(), {type, records}, Type),

    start_lp(Req, type, Since, Type),

    erlang:demonitor(Ref),
    change_relay:unsubscribe(self(), {type, records}, all).

init_change_list_lp(Req, Since)->
    Ref = erlang:monitor(process, change_relay),
    change_relay:subscribe(self(), all, records),

    start_lp(Req, change_list, Since, void),

    erlang:demonitor(Ref),
    change_relay:unsubscribe(self(), {type, records}, all),
    change_relay:unsubscribe(self(), {scope, records}, all).


% send common header chunk and initiate receive for cache updates that
% that represents the end of the longpoll
start_lp(Req, Type, Since, Extra) ->
    Response = Req:respond({200, 
			    [{"Content-Tpye", "application/json;charset=UTF-8"},
			     {"Server","Mochiweb-Longpoll"}],
			    chunked}),
    lp_pusher(Response, Type, Since, Extra).


lp_pusher(Response, Type, Since, Extra) ->
    receive
	{{_, records},{_Operation,#key_to_value{key=K,
						type=T,
						scope=S,
						value=V,
						create_tm=C,
						last_update=U,
						inactive_tm=I,
						ref=R}}} when Type == keys->
	    chunked_response(Response, {ok, {K, V, T, S, C, U, I, R}});

  	{_, {_Operation, _}} when Type == change_list ->
	    Result = jc:change_list(Since),
	    chunked_response(Response, Result);

  	{{_, records}, {_Operation, _}} when Type == scope ->
	    Result = jc:scope_change_list(Extra, Since),
	    chunked_response(Response, Result);

  	{{_, records}, {_Operation, _}} when Type == type->
	    Result = jc:type_change_list(Extra, Since),
	    chunked_response(Response, Result);

	{'DOWN', _MonitorRef, _Type, {change_relay, _Node}, _Info} ->
	    Response:write_chunk(?RESUBSCRIBE),
	    Response:write_chunk(<<>>);

	_Other ->
	    lp_pusher(Response, Type, Since, Extra)
    after
	?LONGPOLL_TIME_OUT_MSEC  ->
	    Response:write_chunk(?RESUBSCRIBE),
	    Response:write_chunk(<<>>)
    end.

chunked_response(Response, Result)->
    JsonResp = json_formatter:format_return(Result),
    Response:write_chunk(JsonResp),
    Response:write_chunk(<<>>).


