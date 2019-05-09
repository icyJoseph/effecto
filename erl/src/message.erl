-module(message).

-export([
	send/3
	]).

send(Id, Group, Message) ->
	case authorize:member(Id, Group) of
		true -> 
			[{_, Members}] = ets:lookup(group, Group),
			io:format("Send message ~p to : ~p\n",[Message, Members]),
			<<"message sent">>;
		_ -> false
	end.