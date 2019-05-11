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
	broadcast(
		Group, jsone:encode(#{<<"joined_meeting">> => Id})
	),
	Info = lists:map(fun({{_, Key}, Value}) -> 
			{Key, Value}
		end, ets:match_object(group, {{Group, '_'}, '_'})),
	{
		reply, 
		{text, jsone:encode([{<<"id">>, Group}|Info])}, 
		Group
	};

%%**********************************************************
%% {"command":"send", "data":{"id":"id1","message":"hejdu","group":"group1"}}
%%**********************************************************
websocket_handle(<<"send">>, Data, Group) ->
	Group = maps:get(<<"group">>, Data),
	Message = jsone:encode(#{
			<<"user">> => maps:get(<<"message">>, Data),
			<<"message">> => maps:get(<<"id">>, Data)
		}),
	binary_to_atom(Group, latin1) ! {msg, Message},
	broadcast(Group, Message),
	{ok, Group};

%%**********************************************************
%% {"command":"create_meeting", "data":{"name":"group1", "agenda":[{"time": 15575601120000, "title":"name"}]}}
%%**********************************************************
websocket_handle(<<"create_meeting">>, Data, _) ->
	Group = base64:encode(crypto:strong_rand_bytes(40)),
	meeting_group:start(
		Group,
		maps:get(<<"name">>, Data),
		maps:get(<<"agenda">>, Data)
	),
	pg2:join(Group, self()),
	Info = lists:map(fun({{_, Key}, Value}) -> 
			{Key, Value}
		end, ets:match_object(group, {{Group, '_'}, '_'})),
	{
		reply, 
		{text, jsone:encode([{<<"id">>, Group}|Info])}, 
		Group
	};

%%**********************************************************
%% {"command":"next"}
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