-module(variables).
-export([
		get/1,
		test/2,
		test_send/3
	]).

-spec get(atom()) -> any().
get(path) -> "../../priv/";
get(ets) -> [user, group];
get(_) -> fail.

%% variables:test("192.168.2.144", 10).
test(Ip, N) ->
	{ok, ConnPid} = gun:open(Ip, 8080),
    gun:ws_upgrade(ConnPid, "/websocketserver"),
	lists:map(fun(_) -> 
		receive 
			_ -> ok
			% Y1 -> io:format("Y1 ~p~n",[Y1])
		end 
	end, lists:seq(1, 2)),
	timer:sleep(1000),
	lists:map(fun(Id) -> 
		gun:ws_send(ConnPid, {text, integer_to_binary(Id)})
	end, lists:seq(1, N)),
	lists:map(fun(_) -> 
		receive
			_ -> ok
			% Y -> io:format("Y ~p~n",[Y])
		end
	end, lists:seq(1, N)),
	Group = test_create_meeting(Ip),
	lists:map(fun(Id) -> 
		spawn(?MODULE, test_send, [Ip, Id, Group])
	end, lists:seq(1, N)).

test_create_meeting(Ip) ->
	{ok, ConnPid} = gun:open(Ip, 8080),
    gun:ws_upgrade(ConnPid, "/websocket"),
	lists:map(fun(_) -> receive _ -> ok end end, lists:seq(1, 2)),
	gun:ws_send(ConnPid, {text, erlang:iolist_to_binary([
		<<"{\"command\":\"create_meeting\", \"data\":{\"name\":\"group1\", \"creator\":\"1\", \"agenda\":[{\"from\": 15575601120000, \"to\": 15575601120000, \"title\":\"name\"}]}}">>
	])}),
	receive
		{gun_ws,_,_,{text,Json}} -> maps:get(<<"id">>, jsone:decode(Json))
	end.

test_send(Ip, N, Group) ->
	{ok, ConnPid} = gun:open(Ip, 8080),
    gun:ws_upgrade(ConnPid, "/websocket"),
	lists:map(fun(_) -> receive _ -> ok end end, lists:seq(1, 2)),
	gun:ws_send(ConnPid, {text, erlang:iolist_to_binary([
		<<"{\"command\":\"join\", \"data\":{\"id\":\"">>,
		integer_to_binary(N),
		<<"\",\"group\":\"">>,
		Group,
		<<"\"}}">>
	])}),
	receive
		_ -> ok
		% X -> io:format("X ~p~n",[X])
	end,
	gun:ws_send(ConnPid, {text, erlang:iolist_to_binary([
		<<"{\"command\":\"send\", \"data\":{\"id\":\"">>,
		integer_to_binary(N),
		<<"\",\"message\":\"hejdu\",\"group\":\"">>,
		Group,
		<<"\"}}">>
	])}),
	receive
		Y -> io:format("Y ~p~n",[Y])
	end.