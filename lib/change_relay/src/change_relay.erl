%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum
%%% @copyright (C) 2011, Jim Rosenblum
%%% @doc Change Realy notifies clients of changes to JCache items and provides
%%% varying degrees of information about the change depending on the subscription
%%% type. 
%%%
%%% Subscription Types
%%% <li> susbscribe(Pid, key, Instance) where Instance = {@link key()}</li>
%%%      <ul><li> Client receives {key, {Instance, write|delete}} </li> </ul>
%%% <li> subscribe(Pid, type|scope, Instance} where Instance = 
%%%                                      {@link type()} or {@link scope()} </li>
%%%      <ul> <li> Client receives {type|scope, {Instance, write|delete}} </li> 
%%%      <li> Client receives ones message per transaction per subscription type,
%%%           so 100 updates and 20 deletes, in one transaction, would result in 
%%%           two client messags </li></ul>
%%% <li> susbscribe(pid, {type|scope, records}, Instance) where 
%%%                                   Instance = {@link type()} or {@link scope()} </li>
%%%      <ul> <li> Client receives {{type|scope, records}, {write|delete, 
%%%          #key_to_value{}}} </li> </ul>
%%% <li> susbscribe(pid, {key, records}, Instance) where Instance = 
%%%                                                          {@link key()} </li>
%%%      <ul><li> Client receives {{key, records}, {write|delete, 
%%%          #key_to_value{}}} </li> </ul>
%%% <li> susbscribe(pid, all, records) </li> 
%%%      <ul><li> Client receives {all, {write|delete, #key_to_value{}}} </li> </ul>
%%% 
%%%
%%%  The change_relay State includes
%%%  the ID of an ETS table containing Subscription information, 
%%%  the ID of an ETS table containing the Pid's of subscribing clients,
%%%  the ID of an ETS table used to collapse multi-row updates that effect
%%%  a single Scope or Type so that subscribers to a Scope or Type get a single
%%%  message when, for example, 27 beds change under scope '3A' rather than 27.
%%%
%%%  Ets table storing subscriptions looks like this
%%%  Key = {Type, Instance}, Value = a set of subscribing Pids
%%%  Ex:   Key = {bed, 1205}, Value = set:[Pid1, Pid2,...Pidn]
%%%              {scope, "3A"}, Value = set:[Pid1, Pid2]
%%%
%%% @end
%%% Created : 16 Sep 2011 by Jim Rosenblum
%%%-----------------------------------------------------------------------------
-module(change_relay).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).


% Module API for starting and stopping the change_relay server
-export([start_link/0, 
	 stop/0]).


% API for interacting with the change_relay's services
-export([subscribe/3, 
	 unsubscribe/3, 
	 load/0, 
	 client_count/0]).


% gen_server callbacks
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, 
	 code_change/3]).


-include("../../include/records.hrl").
-include("../../include/types.hrl").


-define(SERVER, ?MODULE). 
-define(CLEAN_BUFF_MS, 30000).        %Length of time a buf_table row lasts
-define(DEADBEATS_MS, 3600000).       %Length of time to check for deadbeats


-record(state, {sub_table                          :: ets:tid(),
		cli_table                          :: ets:tid(),
		buf_table                          :: ets:tid(),
		clean_buffer_ms = ?CLEAN_BUFF_MS   :: non_neg_integer(),
		evict_deadbeats_ms = ?DEADBEATS_MS :: non_neg_integer()
		
	       }).
-record(coalesce, {key, ttl}).

%%%=============================================================================
%%% Module API 
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the change_relay server and links the caller to it.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%%------------------------------------------------------------------------------
%% @doc Stops the server.
%% @end
%%------------------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
    gen_server:cast(?MODULE, stop).



%%%=============================================================================
%%% Client API to interact with the change_relay server
%%%=============================================================================



%%------------------------------------------------------------------------------
%% Manage subscriptions
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Adds a subscription on behalf of the process indicated by the supplied 
%%  Pid. Pid is subscribing to be notified when <em> Type, Instance </em>
%%  has changed.
%% @end
%%------------------------------------------------------------------------------
-spec subscribe(pid(), scope | type | key | {key | scope | type, records} | all,
		all | key()| scope()| type()) -> ok.

subscribe(ClientPid, Type, Instance) ->
    gen_server:call(?MODULE, {subscribe, ClientPid, Type, Instance}).


%%------------------------------------------------------------------------------
%% @doc The process indicated by the supplied Pid is unsubscribing from changes
%%  to <em> Type, Instance </em>. Instance can be the atom 'all'
%% @end
%%------------------------------------------------------------------------------
-spec unsubscribe(pid(), type(), key | type | scope | all)-> ok.

unsubscribe(ClientPid, Type, Instance) ->
    gen_server:call(?MODULE, {unsubscribe, ClientPid, Type, Instance}).


%%------------------------------------------------------------------------------
%% Varous statisitcs reguarding a change_relay server
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Return an estimate of how much load this change_relay server is under.
%% It counts the number of {Type, Instance} being subscribed to +  the number
%% of Pids that are currently subscribed to anything. Under extreme load this
%% can time-out in which case we return that error
%% @end
%%--------------------------------------------------------------------
-spec load() -> integer() | {ok, integer()} | {error, any()}.

load() ->
    try gen_server:call(?MODULE, load)
    catch
	_Error:Reason ->
	    lager:notice("Load function timed out. ~p",[Reason]),
	    {error, Reason}
    end.


%%------------------------------------------------------------------------------
%% @doc Return the number of distinct, subscribing Pids being served by this
%%  change_relay. Under extreme load this function can time-out.
%% @end
%%------------------------------------------------------------------------------
-spec client_count() -> integer() | {error, any()}.

client_count() ->
    try gen_server:call(?MODULE, client_count)
    catch
	_Error: Reason ->
	    lager:notice("Load function timed out. ~p",[Reason]),
	    {error, Reason}
    end.


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @private
%% @doc Initializes the server by creating a table to store subscriptions, a 
%%  table to cache static location information, a table to contain Pids of
%%  subscribers and a messaging coalescing table. It also subscribes to the 
%%  appropriate Mnesia table changes and starts the timed cleaning of the 
%%  coalesce table.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.

init([]) ->
    process_flag(trap_exit, true), 
    lager:debug("Change Relay initializing"),

    BuffMs = get_env(change_relay, clean_buffer_ms, ?CLEAN_BUFF_MS),
    DBMs = get_env(change_relay, evict_deadbeats_ms, ?DEADBEATS_MS),

    Sid = init_subscriber_table(),
    Cid = init_client_table(),
    Bid = init_buffer_table(),

    subscribe_to_table_changes(),

    schedule_clean_after(BuffMs),
    schedule_deadbeat_eviction(DBMs),

    {ok, #state{sub_table = Sid, 
		cli_table = Cid, 
		buf_table = Bid, 
		clean_buffer_ms = BuffMs,
	        evict_deadbeats_ms = DBMs}}.


%%------------------------------------------------------------------------------
%% @private
%% @doc Create ETS tables to store subscription and subscriber information.
%% @end
%%------------------------------------------------------------------------------
init_subscriber_table() ->
    % default ets table is a set
    ets:new(cr_sub, []).


init_client_table() ->
    % default ets table is a set
    ets:new(cr_cli, []).


init_buffer_table() ->
    % default ets table is a set
    ets:new(cr_buf, [{keypos, #coalesce.key}]).


%%------------------------------------------------------------------------------
%% @private
%% @doc change_relay will get a notification from Mnesia changes to key_to_value.
%% @end
%%------------------------------------------------------------------------------
subscribe_to_table_changes()->
    lager:debug("Subscribing to mnesia table changes"),
    mnesia:subscribe({table, key_to_value, detailed}).


unsubscribe_to_table_changes()->
    lager:debug("Unsubscribing to mnesia table changes"),
    mnesia:unsubscribe({table, key_to_value, detailed}).
    


%%------------------------------------------------------------------------------
%% @private
%% @doc Handling call messages for subscribe, unsubscribe, load and client_clount.
%% @end
%%------------------------------------------------------------------------------
handle_call({subscribe, Pid, Type, Instance}, _From, State) ->
    lager:debug("Adding subscription. Client: ~p, Type: ~p, Instance: ~p",
		[Pid, Type, Instance]),
    add_subscription(State, Pid, Type, Instance),
    {reply, ok, State};

handle_call({unsubscribe, Pid, Type, Instance}, _From, State) ->
    lager:debug("Deleting subscription. Client: ~p, Type: ~p, Instance: ~p",
		[Pid, Type, Instance]),
    del_subscription(State, Pid, Type, Instance),
    {reply, ok, State};

handle_call(load,  _From, State) ->
    lager:debug("Calculating load"),
    {reply, calculate_load(State#state.sub_table), State};

handle_call(client_count,  _From, State) ->
    lager:debug("Counting Clients"),
    {reply, ets:info(State#state.cli_table, size), State}.



%%------------------------------------------------------------------------------
%% @private
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
handle_cast(stop, State) ->
    {noreply, State}.


%%------------------------------------------------------------------------------
%% @private
%% @doc Handle info messaes  where a subscribing client goes away, or a table 
%%  change message arrives from a Mnesia Tables or a message to clean the 
%%  coalescing buffer table.
%% @end
%%------------------------------------------------------------------------------

% client went down, remove their subscriptions
handle_info({'DOWN', _MonitorRef, _Type, Object, _Info}, State) ->
    lager:debug("Client disapeared, ~p:", [Object]),
    remove_subscriber(State, Object),
    {noreply, State};


% Time to remove any coalescing items in the ets that are too old.
handle_info({clean_ets, IntMs}, State) ->
    clean_ets(State#state.buf_table),
    schedule_clean_after(IntMs),
    {noreply, State};

% Time to remove any deadbeats -- pids with no active subscriptions
handle_info({evict_deadbeats, IntMs}, State) ->
    remove_deadbeats(State),
    schedule_deadbeat_eviction(IntMs),
    {noreply, State};


% mnesia table changed, broadcast that change to subscribers
handle_info({mnesia_table_event, {write, key_to_value, Record,
				  _OldRecs, Trx}}, State) ->
    lager:debug("Write event from mnesia. Trx: ~p:", [Trx]),
    broadcast_changes(State, Trx, Record, write),
    {noreply, State};

handle_info({mnesia_table_event, {delete, 
				  key_to_value, _What,
				  [Record], Trx}}, State) ->
    lager:debug("Delete event from mnesia. Trx: ~p:", [Trx]),
    broadcast_changes(State, Trx, Record, delete),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.



%%------------------------------------------------------------------------------
%% @private
%% @doc Unsubscribe to the Mnesia tables, delete ets tables. Probably unnecesary, 
%%  but... 
%% @end
%%------------------------------------------------------------------------------
terminate(_Reason, State) ->
    unsubscribe_to_table_changes(),

    ets:delete(State#state.sub_table),
    ets:delete(State#state.cli_table),
    ets:delete(State#state.buf_table),
    ok.


%%------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%==============================================================================
%%% Internal functio
%%%==============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Use the number of things subscribed to + Pids listening as a proxy for 
%% load.
%% @end
%% -----------------------------------------------------------------------------
calculate_load(Tid)->
    F = fun({{_Type, _Instance}, Pids}, Load) ->
		Load + sets:size(Pids) + 1
	end,

    {ok, {integer, ets:foldl(F, 0, Tid)}}.
    

%%------------------------------------------------------------------------------
%% @private
%% @doc Demonitor a client and remove their Pid from the client ETS.
%% @end
%%------------------------------------------------------------------------------
-spec demonitor_client(#state{}, Client::pid()) -> true.

demonitor_client(State, Pid) ->
    lager:debug("Demonitoring client. Pid: ~p:", [Pid]),
    Tid = State#state.cli_table,

    case ets:lookup(Tid, Pid) of
	[{Pid, Ref}] ->
	    demonitor(Ref, [flush]);
	[] -> ok
    end,
    ets:delete(Tid, Pid).


%%------------------------------------------------------------------------------
%% @private
%% @doc Monitor a client and add their Pid to the client ETS.
%% @end
%%------------------------------------------------------------------------------
-spec monitor_client(#state{}, pid()) -> ok.

monitor_client(State, Pid)->
    lager:debug("Monitoring client. Pid: ~p:", [Pid]),
    Tid = State#state.cli_table,

    case ets:lookup(Tid, Pid) of
	[] ->
	    ets:insert(Tid, {Pid, monitor(process, Pid)});
	[{_Pid, _Ref}] ->
            % process already being monitored
	    ok
    end.
    
%%------------------------------------------------------------------------------
%% @private
%% @doc Look for the key {Type, Instance} in the ets table and add the Pid to 
%%  the list of listeners. Add {Type, Instance} and the Pid to the table if not
%%  already there.
%% @end
%%------------------------------------------------------------------------------
add_subscription(State, Pid, Type, Instance )->
    Tid = State#state.sub_table,

    % Monitor the subscriber, so if it dies the subscriptions are dropped.
    monitor_client(State, Pid),

    case ets:lookup(Tid, {Type, Instance}) of
	[]->
	    ets:insert(Tid, {{Type, Instance}, sets:from_list([Pid])});
	[{{Type, Instance}, Pids}]->
	    ets:insert(Tid, {{Type, Instance}, sets:add_element(Pid, Pids)})
    end.


%%------------------------------------------------------------------------------
%% @private
%% @doc Remove Pid from any place where it exists associated with the key 
%%  {Type, Instance}. If Instance = all, remove the Pid from anywhere the key
%%  {Type, _} exists.
%% @end
%%------------------------------------------------------------------------------

del_subscription(State, Pid, Type, Instance) when Instance =/= all->
    Tid = State#state.sub_table,

    case ets:lookup(Tid, {Type, Instance}) of
	[{{Type, Instance}, Pids}] ->
	    NewSetOfPids = sets:del_element(Pid, Pids),
	    case sets:size(NewSetOfPids) of
		0 ->
		    ets:delete(Tid, {Type, Instance});
		_N ->
		    ets:insert(Tid, {{Type, Instance}, NewSetOfPids})
	    end;
	[]->
	    ok
    end;
del_subscription(State, Pid, Type, Instance) when Instance == all->
    Tid = State#state.sub_table,
    F = fun({{T, I}, Pids}, _Ignore)->
		case T of
		    Type ->
			NewSetOfPids = sets:del_element(Pid, Pids),
			case sets:size(NewSetOfPids) of
			    0 ->
				% no subscribers for Type, so remove alltogether
				ets:delete(Tid, {T, I});
			    _N ->
				ets:insert(Tid, {{T, I}, NewSetOfPids})
			end;
		    _NoMatch ->
			ok
		end
	end,
    ets:foldl(F, [], Tid).



%%------------------------------------------------------------------------------
%% @private
%% @doc Subscriber crashed, so remove its Pid from everywhere.
%% @end
%%------------------------------------------------------------------------------
remove_subscriber(State, Pid) ->
    demonitor_client(State, Pid),

    Tid = State#state.sub_table,

    F = fun({{Object, Instance}, Pids}, _Ignore)->
		NewSetOfPids = sets:del_element(Pid, Pids),
		case sets:size(NewSetOfPids) of
		    0 ->
			% no subscribers for Type, so remove alltogether
			ets:delete(Tid, {Object, Instance});
		    _N ->
			ets:insert(Tid, {{Object, Instance}, NewSetOfPids})
		end
	end,
    ets:foldl(F, [], Tid).


%%------------------------------------------------------------------------------
%% @private
%% @doc Broadcast changes to subscribing pids. But, try to send one message for
%% each transaction per Scope and Type
%% @end
%%------------------------------------------------------------------------------
broadcast_changes(State, Trx, #key_to_value{scope=S, type=T, key=K}=R, Operation)->
    lager:debug("Broadcasting changes: Trx ~p:", [Trx]),
    coalesce_changes(State, Trx, scope, S, Operation),
    coalesce_changes(State, Trx, type, T, Operation),
    broadcast_change(State#state.sub_table, key, K, {K, Operation}),
    broadcast_change(State#state.sub_table, {scope, records}, S, {Operation, R}),
    broadcast_change(State#state.sub_table, {type, records}, T, {Operation, R}),
    broadcast_change(State#state.sub_table, {key, records}, K, {Operation, R}),
    broadcast_change(State#state.sub_table, all, records, {Operation, R}).


 

%%------------------------------------------------------------------------------
%% @private
%% @doc See if {Trx, specific_scope, operation} is in the coalesce table. If so,
%% do nothing, otherwise insert it and send scope-change message to susbscribing
%% pids. Ditto for type.
%% @end
%%------------------------------------------------------------------------------
coalesce_changes(#state{sub_table = Tid, 
			buf_table = Bid, 
			clean_buffer_ms = EtMs}, Trx, Type, Instance, Operation)->

    DtTm = calendar:now_to_datetime(erlang:now()),
    Gsec = (calendar:datetime_to_gregorian_seconds(DtTm)) + (EtMs/1000),
    Rec = #coalesce{key = {Trx, Type, Instance, Operation},
		   ttl = Gsec},
    case ets:insert_new(Bid, Rec) of
	false ->
	    ok;
	_Else ->
	    broadcast_change(Tid, Type, Instance, {Instance, Operation})
    end.
	    
broadcast_change(Tid, Type, Instance, Payload)->
    case ets:lookup(Tid, {Type, Instance}) of
	[{{Type, Instance}, Set}] ->
	    send_to_pids(Set, {Type, Payload});
	_else ->
	    ok
    end.

%--------------------------------------------------------
% Send the Message to all of the Pids in the list Pids.
%--------------------------------------------------------
send_to_pids(Pids, Message) ->
    sets:fold(fun(P, _) -> P ! Message, [] end, [], Pids).



%%------------------------------------------------------------------------------
%% @private
%% @doc Create the message that gets sent back to change_relay to trigger a 
%% cleaning of the ets buffer table used to coalesce individual row changes 
%% into single messages to subsribers. 
%% @end
%%------------------------------------------------------------------------------
-spec schedule_clean_after(integer()) -> TimerRef::reference().

schedule_clean_after(IntMs)->
    erlang:send_after(IntMs, ?MODULE, {clean_ets, IntMs}).


%%------------------------------------------------------------------------------
%% @private
%% @doc Create the message that gets sent back to change_relay to trigger a 
%% an eviction of any clients that have no active subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec schedule_deadbeat_eviction(integer()) -> ok.

schedule_deadbeat_eviction(IntMs)->
    erlang:send_after(IntMs, ?MODULE, {evict_deadbeats, IntMs}),
    ok.


%%------------------------------------------------------------------------------
%% @private
%% @doc Remove unnecesry lines from the coalesing table by assuming that old ones 
%% represent messages already sent to subscribers.
%% @end
%%------------------------------------------------------------------------------
-spec clean_ets(atom()) -> integer().

clean_ets(Bid) ->
    DtTm = calendar:now_to_datetime(erlang:now()),
    Gsec = calendar:datetime_to_gregorian_seconds(DtTm),
    lager:debug("Cleaning buffer table of records older than ~p:", [Gsec]),
    ets:select_delete(Bid, [ 
			     {#coalesce{key = '$1', ttl = '$2'},
			      [{'<','$2', Gsec}],
			      [true]}]).


%%------------------------------------------------------------------------------
%% @private
%% @doc Remove clients that have no active subscriptions.
%% @end
%%------------------------------------------------------------------------------
-spec remove_deadbeats(#state{}) -> ok.

remove_deadbeats(#state{sub_table=SubTid, cli_table=CliTid} = State) ->
    lager:debug("Looking for deadbeats..."),
    PidsWithSubs = ets:foldl(fun({_, Set}, Acc) -> sets:union(Set, Acc) end,
			     sets:new(),
			     SubTid),

    F = fun({Pid, _}, Acc) ->
		case sets:is_element(Pid, PidsWithSubs) of
		    true  -> Acc;
		    false -> [Pid, Acc]
		end
	end,
    DeadBeats = ets:foldl(F, [], CliTid),
    [remove_subscriber(State, Pid) || Pid<-DeadBeats],
    ok.
    
    
get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.


