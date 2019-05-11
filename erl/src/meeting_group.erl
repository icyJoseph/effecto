-module(meeting_group).

-export([start/2]).
-export([init/2]).
-export([time/1]).

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

init(_Group, Agenda) -> 
	Agenda1 = lists:map(fun(Entry) -> 
            {
                maps:get(<<"time">>, Entry),
                maps:get(<<"title">>, Entry)
            }
        end, Agenda),
    spawn_link(?MODULE, time, [Agenda1]),
    loop().

%%**********************************************************
%% Receive messages to meeting group
%%**********************************************************
loop() ->
    receive 
        _ -> ok
    end,
    loop().

%%**********************************************************
%% Keep track of agenda
%%**********************************************************
time([{_Time, _Entry}|T]) ->
    receive 
        done -> time(T)
    end.