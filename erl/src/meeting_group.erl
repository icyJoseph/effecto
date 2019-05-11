-module(meeting_group).

-export([start/2]).
-export([init/2]).
-export([time/1]).

%%**********************************************************
%% Start a mailbox for each new group. This mailbox starts 
%% other processes, keeping track of e.g. time and group 
%% members.
%%**********************************************************
% -spec start(Group, Agenda) -> no_return().
start(Group, Agenda) ->
	pg2:create(Group),
    ets:insert(messages_group,{Group, []}),
    register(
        Group, 
        spawn_link(?MODULE, init, [Group, Agenda])
    ).

% -spec init(Group, Agenda) -> no_return().
init(Group, Agenda) -> 
	Agenda1 = lists:map(fun(Entry) -> 
            {
                maps:get(<<"time">>, Entry),
                maps:get(<<"title">>, Entry)
            }
        end, Agenda),
    spawn_link(?MODULE, time, [Agenda1]),
    loop(Group, []).
    
%%**********************************************************
%% Receive messages to meeting group
%%**********************************************************
loop(Group,List) ->
    ListNew = receive 
        {send, Message} -> 
            broadcast(Group, Message),
            ets:insert(messages_group, {Group, [Message | List]}),
            [Message | List];
        {user_joined, Message} ->
            broadcast(Group, Message), List;
        _ -> List
    end,
    loop(Group,ListNew).

%%**********************************************************
%% Keep track of agenda
%%**********************************************************
time([{_Time, _Entry}|T]) ->
    receive 
        done -> time(T)
    end.

%%**********************************************************
%% 
%%**********************************************************
broadcast(Group, Message) ->
    lists:map(fun(Pid) -> 
        Pid ! Message 
    end, pg2:get_members(Group)).