-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([run/2]).

init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(_State) -> {ok, self(), hibernate}.

websocket_handle({text, Msg}, State) ->
	Json = jsone:decode(Msg),
	Data = maps:get(<<"data">>, Json),
	Command = maps:get(<<"command">>, Json),
	Re = run(Command, Data),
	{reply, {text, Re}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
	{ok, State, hibernate};

websocket_info(_Info, State) ->
	{ok, State}.

%% {"command":"login", "data":{"id":"id1","ip":"ip1"}}
run(<<"login">>, Data) ->
	io:format("login\n"),
	Id = maps:get(<<"id">>, Data),
	Ip = maps:get(<<"ip">>, Data),
	case authorize:login(Id, Ip) of
		true -> <<"logged in">>;
		false -> <<"error - could not login">>
	end;
%% {"command":"invite", "data":{"id":"id1","group":"group1","ip":"ip1"}}
run(<<"invite">>, Data) ->
	io:format("invite\n"),
	Id = maps:get(<<"id">>, Data),
	Group = maps:get(<<"group">>, Data),
	Ip = maps:get(<<"ip">>, Data),
	authorize:invite(Id, Ip, Group),
	<<"invited">>;

%% {"command":"send", "data":{"id":"id1","message":"group1"}}
run(<<"send">>, Data) ->
	io:format("send\n"),
	Id = maps:get(<<"id">>, Data),
	Message = maps:get(<<"message">>, Data),
	[{_, Group}] = ets:lookup(online, Id),
	message:send(Id, Group, Message).