-module(meeting_group).

-export([start/2]).
-export([init/2]).

%%**********************************************************
%% Start a mailbox for each new group. This mailbox starts 
%% other processes, keeping track of e.g. time and group 
%% members.
%%**********************************************************
start(Group, Agenda) ->
	pg2:create(Group),
    register(
        binary_to_atom(Group, latin1), 
        spawn_link(?MODULE, init, [Group, Agenda])
    ).

init(Group, Agenda) -> 
    io:format("Group ~p~n",[Group]),
    io:format("Agenda ~p~n",[Agenda]),
    loop().

loop() ->
    receive 
        _ -> ok
    end,
    loop().