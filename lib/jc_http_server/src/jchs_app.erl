%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim
%%% @doc Application module for the jc_http_server application.
%%%
%%% @end
%%% Created : 16 Oct 2011 by Jim <>
%%%-------------------------------------------------------------------
-module(jchs_app).

-behaviour(application).

-define(WAIT_FOR_RESOURCES, 2500).
-define(DEFAULT_PORT, 3000). 

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Starts the jc_http_server  application by calling jchs_sup.
%% @spec start(_StartType, _StartArgs) -> {ok, Pid} |
%%                                        {error, Reason}
%% @end
%%------------------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    Port = get_env(jc_http_server, port, ?DEFAULT_PORT),
    case jchs_sup:start_link([Port]) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever the application has stopped.
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.
