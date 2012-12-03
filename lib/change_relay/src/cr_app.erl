%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc Application module for the change_relay application.
%%%
%%% @end
%%% Created : 16 Oct 2011 by Jim <>
%%%-------------------------------------------------------------------
-module(cr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Starts the change_replay application by calling cr_sup, the 
%%      top-level supervisor for the application.
%% @spec start(_StartType, _StartArgs) -> {ok, Pid} |
%%                                        {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case cr_sup:start_link() of
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
