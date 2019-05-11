-module(server_ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
	% {{192,168,2,145},_} = maps:get(peer, Req),
	% {{127,0,0,1}, _} = maps:get(peer, Req),
	{
		cowboy_websocket, Req, Opts, 
		#{idle_timeout => 1000 * 60 * 60 * 24}
	}.

websocket_init(_State) -> {ok, self(), hibernate}.

%%**********************************************************
%% id
%%**********************************************************
websocket_handle({text, Id}, State) ->
	ets:insert(user, {{Id, auth}}),
	{reply, {text, <<"invited">>}, State};

websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, _Msg}, State) ->
	{ok, State, hibernate};

websocket_info(_Info, State) ->
	{ok, State}.