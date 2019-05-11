-module(meeting_group).

-export([start/1]).
-export([init/0]).

%%**********************************************************
%% Start a mailbox for each new group. This mailbox starts 
%% other processes, keeping track of e.g. time and group 
%% members.
%%**********************************************************
start(Group) ->
	pg2:create(Group),
    register(
        binary_to_atom(Group, latin1), 
        spawn_link(?MODULE, init, [])
    ).

init() -> loop().

loop() ->
    receive 
        _ -> ok
    end,
    loop().