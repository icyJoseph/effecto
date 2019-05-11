-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{
		cowboy_websocket, Req, Opts, 
		#{idle_timeout => 1000 * 60 * 60 * 24}
	}.

websocket_init(_State) -> {ok, self(), hibernate}.

websocket_handle({text, Msg}, State) ->
	Json = jsone:decode(Msg),
	Data = maps:get(<<"data">>, Json),
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
	false = lists:member(Id, pg2:get_members(Group)),
	pg2:join(Group, self()),
	register(binary_to_atom(Id, latin1), self()),
	broadcast(
		Group, jsone:encode(#{<<"joined_meeting">> => Id})
	),
	{ok, Group};

%%**********************************************************
%% {"command":"send", "data":{"id":"id1","message":"hejdu","group":"group1"}}
%%**********************************************************
websocket_handle(<<"send">>, Data, Group) ->
	Group = maps:get(<<"group">>, Data),
	Message = jsone:encode(#{
			<<"user">> => maps:get(<<"message">>, Data),
			<<"id">> => maps:get(<<"id">>, Data)
		}),
	broadcast(Group, Message),
	{ok, Group};

%%**********************************************************
%% {"command":"create_meeting", "data":{"name":"group1", "agenda":[{"time": "1557560112000", "title":"name"}]}}
%%**********************************************************
websocket_handle(<<"create_meeting">>, Data, _) ->
	Group = maps:get(<<"name">>, Data),
	meeting_group:start(
		Group, 
		maps:get(<<"agenda">>, Data)
	),
	pg2:join(Group, self()),
	{reply, {text, <<"A new meeting has been created">>}, Group}.

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