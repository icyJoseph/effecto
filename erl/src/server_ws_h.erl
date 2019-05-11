-module(server_ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	{{127,0,0,1}, _} = maps:get(peer, Req),
	{
		cowboy_websocket, Req, Opts, 
		#{idle_timeout => 1000 * 60 * 60 * 24}
	}.

websocket_init(_State) -> {ok, self(), hibernate}.

%%**********************************************************
%% 
%%**********************************************************
websocket_handle({text, Data}, State) ->
	Id = maps:get(<<"id">>, Data),
	Group = maps:get(<<"group">>, Data),
	database:insert(user, {Id, Group}),
	{reply, {text, <<"invited">>}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
	{ok, State, hibernate};

websocket_info(_Info, State) ->
	{ok, State}.