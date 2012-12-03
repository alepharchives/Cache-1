%%%-----------------------------------------------------------------------------
%%% @author  <Jim Rosenblum>
%%% @copyright (C) 2012, 
%%% @doc I thought it would be a good idea to have a module of one exported 
%%% function, format_return/1, that takes results from the jc and / or 
%%% jc_http_server module and converts them to json. The idea is that I could
%%% have a different module exporting the same fn that would produce xml, for 
%%% example. Not sure if this is actually a good idea. 
%%% @end
%%% Created :  7 Oct 2012 by  <Jim Rosenblum>
%%%-----------------------------------------------------------------------------

-module(json_formatter).

-export([format_return/1]).


% macros for stock-responses
-define(OK, <<"{\"response\":\"ok\"}">>).
-define(QUIP, <<"\"...JCache is up\"">>).
-define(KEY_EXISTS, <<"{\"info\":\"key_exists\"}">>).
-define(NO_TYPE, <<"{\"info\":\"type_not_found\"}">>).
-define(NO_SCOPE, <<"{\"info\":\"scope_not_found\"}">>).
-define(RESUBSCRIBE, <<"{\"error\":\"resubscribe\"}">>).
-define(BAD_JSON, <<"{\"info\":\"bad json in put or post\"}">>).
-define(NOT_FOUND, <<"{}">>).


format_return(all_is_well) ->
    Time = jc_util:now_to_Uepoch(),
    [<<"{\"time\":">>,
     integer_to_binary_string(Time),
     <<", \"relax\":">>,
     ?QUIP,
    <<"}">>];

format_return(ok) ->
     ?OK;

format_return({ok, []}) ->
     ?NOT_FOUND;

format_return({error, not_found}) ->
    ?NOT_FOUND;

format_return({error, scope_not_found}) ->
    ?NOT_FOUND;

format_return({error, type_not_found}) ->
    ?NOT_FOUND;

format_return({error, Error}) when is_atom(Error) ->
    [<<"{\"error\":\"">>, atom_to_binary(Error, utf8), <<"\"}">>];

format_return({error, {unknown_action, Action}}) ->
    [<<"{\"error\":{\"unknown_action\":\"">>, 
     list_to_binary(Action), 
     <<"\"}}">>];


format_return({uptime, List}) ->
    [{up_at,UP},
     {now, Now},
     {up_time, {D, {H, M, S}}}] = List,
    Time = jc_util:now_to_Uepoch(),
    [<<"{\"time\":">>, integer_to_binary_string(Time),
     <<", \"stats\":[{\"up_at\":\"">>, make_binary(UP), 
     <<"\"}, {\"now\":\"">>,make_binary(Now),
     <<"\"}, {\"up_time\":[">>, 
     <<"{\"days\":">>, integer_to_binary_string(D), <<"},">>, 
     <<"{\"hours\":">>, integer_to_binary_string(H), <<"},">>, 
     <<"{\"minutes\":">>, integer_to_binary_string(M), <<"},">>,
     <<"{\"seconds\":">>, integer_to_binary_string(S), <<"}">>,
      <<"]}]}">>];

format_return({resources, List}) ->
    Time = jc_util:now_to_Uepoch(),
    Struct = {struct, [{time, Time}, {cache_resources, List}]},
    mochijson2:encode(Struct);

format_return({size, Data}) ->
    Time = jc_util:now_to_Uepoch(),
    TableInfo = [{struct,[{T,{struct,[{records,R}, {words, W}]}}]} || 
		    {T,R,W} <- Data],
    Struct = {struct, [{time, Time}, {sizes, TableInfo}]},
    mochijson2:encode(Struct);

format_return({ok, {Fld, Value}}) when
      Fld == integer; Fld == value; Fld == scope; Fld == type ->
    Time = jc_util:now_to_Uepoch(),
    Struct = jc_field_2_struct(Fld, Value),
    Struct2 = {struct, [{time, Time}, Struct]},
    mochijson2:encode(Struct2);

format_return({ok, ResultList}) when is_list(ResultList) ->
    jc_elements_2_json(ResultList);

format_return({changelist, {Add, Update, Delete}}) ->
    Time = jc_util:now_to_Uepoch(),
    Struct = [{struct, [{<<"removedData">>, elements_2_struct(Delete)},
			{<<"addedData">>, elements_2_struct(Add)}, 
			{<<"changedData">>, elements_2_struct(Update)}]}], 
    Struct2 = {struct, [{time, Time}, {changeList, Struct}]},
    mochijson2:encode(Struct2);

format_return({changelists, ChangeList}) ->
    Time = jc_util:now_to_Uepoch(),
    Struct = [{struct, [{modelType, make_binary(MT)},
			{<<"removedData">>, elements_2_struct(Delete)},
			{<<"addedData">>, elements_2_struct(Add)}, 
			{<<"changedData">>, elements_2_struct(Update)}]} ||
	      {MT, {Add, Update, Delete}} <- ChangeList], 
    Struct2 = {struct, [{time, Time}, {changeList, Struct}]},
    mochijson2:encode(Struct2);

format_return({plymouth, UpdateType, ChangeList}) ->
    Time = jc_util:now_to_Uepoch(),
    Struct = [{struct, [{<<"removedData">>, elements_22_struct(Delete)},
			{<<"addedData">>, elements_22_struct(Add)}, 
			{<<"updatedData">>, elements_22_struct(Update)},
			{<<"updateType">>, UpdateType},
			{modelName, MT}]} ||
	      {MT, {Add, Update, Delete}} <- ChangeList], 
    Struct2 = {struct, [{time, Time}, {changeLists, Struct}]},
    mochijson2:encode(Struct2);

format_return({{session_id, Sid}, {plymouth, UpdateType, ChangeList}}) ->
    Struct = [{struct, [{<<"removedData">>, elements_22_struct(Delete)},
			{<<"addedData">>, elements_22_struct(Add)}, 
			{<<"updatedData">>, elements_22_struct(Update)},
			{modelName, MT},
			{<<"updateType">>, UpdateType}]} ||
		 {MT, {Add, Update, Delete}} <- 
		     ChangeList, 
		 UpdateType == <<"Full">> orelse
		     Add /= [] orelse
		     Update /= [] orelse
		     Delete /= [] 
	     ], 
    Struct2 = {struct, [{clientSessionId, Sid}, 
			{changeLists, Struct}, 
			{updateType, UpdateType},
			{nextUpdate, 10000}]},
    mochijson2:encode(Struct2);


format_return({{value, Value}, {url, Url}}) ->
    Time = jc_util:now_to_Uepoch(),
    ValStruct = jc_field_2_struct(value, Value),
    UrlStruct = [make_binary(Url)],
    Struct = {struct, [{time, Time}, 
		       ValStruct,
		       {urls, [UrlStruct]}]},
    mochijson2:encode(Struct);

format_return({{key, Value}, {url, Url}})->
    Time = jc_util:now_to_Uepoch(),
    UrlStruct = [make_binary(Url)],
    Struct = {struct, [{time, Time},
		       {key, make_binary(Value)},
		       {urls, [UrlStruct]}]},
    mochijson2:encode(Struct);


format_return({{integer, Int}, {url, Url}}) ->
    Time = jc_util:now_to_Uepoch(),
    ValStruct = {integer, {struct, [{value, Int}]}},
    UrlStruct = [make_binary(Url)],
    Struct = {struct, [{time, Time}, 
		       ValStruct,
		       {urls, [UrlStruct]}]},
    mochijson2:encode(Struct);

format_return({Operation, {url, Url}}) when is_list(Url)->
    Time = jc_util:now_to_Uepoch(),
    UrlStruct = [make_binary(AnUrl) || AnUrl <- Url],
    Struct = {struct, [{time, Time},
		      {operation, Operation},
		      {urls, [UrlStruct]}]},
    mochijson2:encode(Struct);

format_return({ok, Rec}) when is_tuple (Rec) ->
    jc_elements_2_json([Rec]);

format_return({key, {Instance, Op}}) ->
    Time = jc_util:now_to_Uepoch(),
    Resp = {struct, [{time, Time},
		     {<<"subscriptionType">>, <<"key">>},
		     {<<"operation">>, make_binary(Op)},
		     {<<"item">>, make_binary(Instance)}
		    ]},
    mochijson2:encode(Resp);


format_return({scope, {Instance, Op}}) ->
    Time = jc_util:now_to_Uepoch(),
    Resp = {struct, [{time, Time},
		     {<<"subscriptionType">>,<<"scope">>}, 
		     {<<"operation">>, make_binary(Op)},
		     {<<"item">>, make_binary(Instance)}
		    ]}, 
    mochijson2:encode(Resp);


format_return({type, {Instance, Op}}) ->
    Time = jc_util:now_to_Uepoch(),
    Resp = {struct, [{time, Time},
		     {<<"subscriptionType">>,<<"type">>}, 
		     {<<"operation">>, make_binary(Op)},
		     {<<"item">>, make_binary(Instance)}
		    ]}, 
    mochijson2:encode(Resp);


format_return(Any) when is_list(Any)-> 
    [<<"{\"unknown_error\":\"">>, list_to_binary(Any)];

format_return(Any) -> 
    io_lib:fwrite("{\"unknown_error\":\"~p\"}",[Any]).


%%==============================================================================
%% Formatting helper functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% Take a field returned by jc:_ and wrap it it in a mochiweb2 structure for 
%% conversion to JSON
%%------------------------------------------------------------------------------

jc_field_2_struct(FieldNm, Value) ->
    V1 = case FieldNm of
	     value -> {json, Value};
	     _       -> make_binary(Value)
	 end,
    {field, {struct, [{FieldNm, V1}]}}.



%%------------------------------------------------------------------------------
%% Map the list of cache elements, the return from a JCache fn, into JSON
%% via mochijson2.
%%------------------------------------------------------------------------------

jc_elements_2_json(CacheResults) ->
    Time = jc_util:now_to_Uepoch(),
    Struct = elements_2_struct(CacheResults),
    Struct2 = {struct, [{time, Time},
			{elements, Struct} ] },
    mochijson2:encode(Struct2).



%%------------------------------------------------------------------------------
%% Produce the mochijson2 structure that represents an array of returned cache
%% elements. {json, V1} results in V1 being added to the struct without any
%% kind of encoding or decoding which is good because V1 is already json
%%

elements_2_struct([]) -> [];
elements_2_struct(Results) -> 
    [{struct, [{make_binary(K),{struct, [{<<"value">>, {json, V1}}, 
					 {<<"type">>,make_binary(T)},
					 {<<"scope">>,make_binary(S)},
					 {<<"create_tm">>, make_binary(C)},
					 {<<"last_update">>, make_binary(U)},
					 {<<"inactive">>, make_binary(I)}
					]}}]} || 
	{K, V1, T, S, C, U, I, _} <-Results].

elements_22_struct([]) -> [];
elements_22_struct(Results) -> 
    [{json, V1}|| {_K, V1, _T, _S, _C, _U, _I, _} <-Results].

    

%%------------------------------------------------------------------------------
%% convert terms to binary which is what the mochijon2 functions require.
%%

make_binary(true) -> true;
make_binary(false) -> false;
make_binary(null) -> null;
make_binary(T) when is_binary(T) -> T;
make_binary(T) when is_integer(T) -> T;
make_binary(T) when is_number(T) -> T; 
make_binary(T) when is_atom(T) -> list_to_binary(atom_to_list(T));
make_binary(T) when is_list(T) -> 
    case catch list_to_binary(T) of
	{'EXIT', _R} ->  ?BAD_JSON;
	Binary -> Binary
    end;
make_binary(_T) ->  ?BAD_JSON.


integer_to_binary_string(T) when is_integer(T) ->
    list_to_binary(integer_to_list(T)).


    
