-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([broadcast/2]).

init(Req, Opts) ->
	{
		cowboy_websocket, Req, Opts, 
		#{idle_timeout => 1000 * 60 * 60 * 24}
	}.

websocket_init(_State) -> {ok, self(), hibernate}.

websocket_handle({text, Msg}, State) ->
	Json = jsone:decode(Msg),
	Data = maps:get(<<"data">>, Json, #{}),
	Command = maps:get(<<"command">>, Json),
	websocket_handle(Command, Data, State);

websocket_handle(_Data, State) ->
	{ok, State}.

%%**********************************************************
%% {"command":"join", "data":{"id":"id1","group":"group1"}}
%%**********************************************************
websocket_handle(<<"join">>, Data, _) ->
	Id = maps:get(<<"id">>, Data),
	[{{Id, auth}}] = ets:lookup(user, {Id, auth}),
	Group = maps:get(<<"group">>, Data),
	ets:insert(user, {{Id, Group}}),
	pg2:join(Group, self()),
	(catch register(binary_to_atom(Id, latin1), self())),
	Info = lists:map(fun({{_, Key}, Value}) -> 
			{Key, Value}
		end, ets:match_object(group, {{Group, '_'}, '_'})),
	{
		reply, 
		{text, jsone:encode([
			{<<"joined_meeting">>, true}, 
			{<<"id">>, Group}|Info
		])}, 
		Group
	};

%%**********************************************************
%% {"command":"send_feeling", "data":{"id":"id1","feeling":"happy","group":"group1"}}
%%**********************************************************
websocket_handle(<<"send_feeling">>, Data, Group) ->
	Group = maps:get(<<"group">>, Data),
	Message = jsone:encode(#{
			<<"user">> => maps:get(<<"id">>, Data),
			<<"feeling">> => maps:get(<<"feeling">>, Data)
		}),
	binary_to_atom(Group, latin1) ! {msg, Message},
	broadcast(Group, Message),
	{ok, Group};

%%**********************************************************
%% {"command":"send", "data":{"id":"id1","message":"hejdu","group":"group1"}}
%%**********************************************************
websocket_handle(<<"send">>, Data, _Group) ->
	io:format("Data ~p~n",[Data]),
	% io:format("Group ~p~n",[Group]),
	Group = maps:get(<<"group">>, Data),
	Message = jsone:encode(#{
			<<"user">> => maps:get(<<"id">>, Data),
			<<"message">> => maps:get(<<"message">>, Data)
		}),
	binary_to_atom(Group, latin1) ! {msg, Message},
	broadcast(Group, Message),
	{ok, Group};

%%**********************************************************
%% {"command":"start_meeting", "data":{"group":"group1"}}
%%**********************************************************
websocket_handle(<<"start_meeting">>, Data, Group) ->
	Group = maps:get(<<"group">>, Data),
	binary_to_atom(Group, latin1) ! start_meeting,
	broadcast(Group, <<"meeting started">>),
	{ok, Group};

%%**********************************************************
% {"command":"create_meeting", "data":{"name":"group1", "creator":"id1","agenda":[{"from": 1557584366000,"to": 1557584396000, "title":"name1"},{"from": 1557584366000,"to": 1557584396000, "title":"name2"}]}}
%%**********************************************************
websocket_handle(<<"create_meeting">>, Data, _) ->
	G = base64:encode(crypto:strong_rand_bytes(40)),
	Group = binary:part(G, {1, 5}),
	meeting_group:start(
		Group,
		maps:get(<<"name">>, Data),
		maps:get(<<"creator">>, Data),
		maps:get(<<"purpose">>, Data),
		maps:get(<<"agenda">>, Data)
	),
	pg2:join(Group, self()),
	Info = lists:map(fun({{_, Key}, Value}) -> 
			{Key, Value}
		end, ets:match_object(group, {{Group, '_'}, '_'})),
	{
		reply, 
		{text, jsone:encode([
			{<<"id">>, Group}, {<<"success">>, true}|Info])}, 
		Group
	};

%%**********************************************************
%% {"command":"next", "data":{"group":"GrupeID"}}
%%**********************************************************
websocket_handle(<<"next">>, Data, Group) ->
	Group = maps:get(<<"group">>, Data),
	binary_to_atom(Group, latin1) ! next_agenda,
	broadcast(Group, <<"next_task">>),
	{ok, Group}.

websocket_info({timeout, _Ref, _Msg}, State) ->
	{ok, State, hibernate};

websocket_info(Info, State) ->
	{reply, {text, Info}, State}.

%%**********************************************************
%% 
%%**********************************************************
broadcast(Group, Message) ->
	lists:map(fun(Pid) -> 
		Pid ! Message 
	end, lists:usort(pg2:get_members(Group))).