-module(authorize).

-export([
	init/0,
	invite/3,
	login/2,
	member/2,
	test/0
	]).

test() ->
	init(),
	invite(<<"id1">>, <<"ip1">>, <<"group1">>),
	login(<<"id1">>, <<"ip1">>),
	invite(<<"id2">>, <<"ip2">>, <<"group1">>),
	login(<<"id2">>, <<"ip2">>),
	login(<<"id3">>, <<"ip3">>).

init() ->
	ets:new(online, [ordered_set, named_table, public]),
	%% todo: online should propaly include a PID to the 
	%% direct ws connection for the user.
	ets:new(invite, [ordered_set, named_table, public]),
	ets:new(group, [ordered_set, named_table, public]),
	ok.

invite(Id, Ip, Group) ->
	ets:insert(invite, {Id, Ip, Group}).

login(Id, Ip) ->
	case ets:match(invite, {Id, Ip, '$1'}) of
		[[Group]] -> 
			ets:delete(invite, Id),
			ets:insert(online, {Id, Group}),
			case ets:lookup(group, Group) of
				[] -> ets:insert(group, {Group, [Id]});
				[{Group, ListOfIds}] -> ets:insert(group, {Group, [Id | ListOfIds]})
				%% TODO start a new ws connection with computer that are trying to login.
			end, true;
		_ -> false
	end. 

member(Id, Group) ->
	case ets:lookup(group, Group) of
		[] -> false;
		[{_, Members}] -> lists:member(Id, Members)
	end.

