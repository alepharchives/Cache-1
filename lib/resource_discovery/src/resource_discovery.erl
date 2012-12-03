%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc Implements a Simple Resource Discovery service where
%%%      Resource       =  A concrete resource (a fun, etc.) or a 
%%%                        reference to a concrete resource (pid).
%%%      Type           =  a tag used to classify resources
%%%      Resource Tuple = {Type,  Resource}
%%% @end
%%% Created : 18 Oct 2011 by Jim Rosenblum
%%%-------------------------------------------------------------------
-module(resource_discovery).

-behaviour(gen_server).


%% API
-export([start_link/0,
	 add_target_resource_type/1,
	 add_local_resource/2,
	 fetch_resources/1,
	 trade_resources/0,
	 init_found_and_trade_resources/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {target_resource_types,    % what I am looking for
		local_resource_tuples,    % what I offer
		found_resource_tuples}).  % what I have found


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc Adds a Type to the list of target resources -- "I want Type".
%% @spec add_target_resource_type(Type) -> ok
%%       where Type = atom()
%% @end
%%--------------------------------------------------------------------
add_target_resource_type(Type) ->
    gen_server:cast(?SERVER, {add_target_resource_type, Type}).


%%--------------------------------------------------------------------
%% @doc Adds a Resource Tuple to the list of resources available at
%%      this node -- "I have {Type, Instance}".
%% @end
%%--------------------------------------------------------------------
-spec add_local_resource(atom(), atom()) ->ok.

add_local_resource(Type, Instance) ->
    gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).


%%-----------------------------------------------------------------------------
%% @doc Returns all of the instances discovered for the given Type
%%      *IF* the type is one that is being looked for -- in the "I want
%%       list".
%% @end
%%------------------------------------------------------------------------------
-spec fetch_resources(atom()) -> {resources, [atom()]} | {error, no_resources}.

fetch_resources(Type) ->
    gen_server:call(?SERVER, {fetch_resources, Type}).



%%--------------------------------------------------------------------
%% @doc Triggers resource trading.
%%
%% @spec trade_resources() -> ok
%% @end
%%--------------------------------------------------------------------
trade_resources() ->
    gen_server:cast(?SERVER, trade_resources).

%%--------------------------------------------------------------------
%% @doc Reinitializes found_resources and triggers resource 
%% trading.
%% @spec init_found_and_trade_resources() -> ok
%% @end
%%--------------------------------------------------------------------
init_found_and_trade_resources() ->
    gen_server:call(?SERVER, init_found_resources),
    gen_server:cast(?SERVER, trade_resources).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server. 
%% @spec init([]) -> {ok, State} 
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{target_resource_types = [],
		local_resource_tuples = dict:new(),
		found_resource_tuples = dict:new()}}.



%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({fetch_resources, Type}, _From, State) ->
    R = case dict:find(Type, State#state.found_resource_tuples) of
	    error         -> {error, no_resource};
	    {ok, Value} -> {resources, Value}
	end,
    {reply, R, State};

handle_call(init_found_resources, _From, State)->
    {reply, ok, State#state{found_resource_tuples = dict:new()}};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({add_target_resource_type, Type}, State) ->
    TargetTypes = State#state.target_resource_types,
    NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
    {noreply, State#state{target_resource_types = NewTargetTypes}};


handle_cast(trade_resources, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    AllNodes = [node() | nodes()],
    lists:foreach(
      fun(Node) ->
	      gen_server:cast({?SERVER, Node},
			      {trade_resources, {node(), ResourceTuples}})
      end,
      AllNodes),
    {noreply, State};


handle_cast({add_local_resource, {Type, Instance}}, State) ->
    ResourceTuples = State#state.local_resource_tuples,
    NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
    {noreply, State#state{local_resource_tuples = NewResourceTuples}};


handle_cast({trade_resources, {ReplyTo, Remotes}},
	    #state{local_resource_tuples = Locals,
		   target_resource_types = TargetTypes,
		   found_resource_tuples = OldFound} = State) ->
    FilteredRemotes = resources_for_types(TargetTypes, Remotes),
    NewFound = add_resources(FilteredRemotes, OldFound),
    case ReplyTo of
	noreply ->
	    ok;
	_ ->
	    gen_server:cast({?SERVER, ReplyTo},
			    {trade_resources, {noreply, Locals}})
    end,
    {noreply, State#state{found_resource_tuples = NewFound}};


handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_resource(Type, Resource, ResourceTuples) ->
    case dict:find(Type, ResourceTuples) of
	{ok, ResourceList} ->
	    NewList = [Resource | lists:delete(Resource, ResourceList)],
	    dict:store(Type, NewList, ResourceTuples);
	error ->
	    dict:store(Type, [Resource], ResourceTuples)
    end.



add_resources([{Type, Resource} | T], ResourceTuples) ->
    add_resources(T, add_resource(Type, Resource, ResourceTuples));

add_resources([], ResourceTuples) ->
    ResourceTuples.


resources_for_types(Types, ResourceTuples) ->
    Fun =
	fun(Type, Acc) ->
		case dict:find(Type, ResourceTuples) of
		    {ok, List} ->
			[{Type, Instance} || Instance <- List] ++ Acc;
		    error ->
			Acc
		end
	end,
    lists:foldl(Fun, [], Types).
