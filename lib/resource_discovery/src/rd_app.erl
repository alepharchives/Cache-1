%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim
%%% @doc Appliction module for the resource_discovery application
%%%
%%% @end
%%% Created : 19 Oct 2011 by Jim Rosenblum
%%%-------------------------------------------------------------------
-module(rd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Starts the resource_discovery application by calling the rd_sup,
%%      the top-level supervisor for the application.
%%
%% @spec start(_StartType, _StartArgs) -> {ok, Pid} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case rd_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. 
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
