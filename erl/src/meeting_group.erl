%%**********************************************************
%% TODO:
%% Add meeting host
%%**********************************************************

-module(meeting_group).

-export([start/2]).
-export([init/2]).
-export([time/2]).
-export([reminder/2]).

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
    {ok, TimePid} = spawn_link(?MODULE, time, [Group, Agenda1]),
    loop(Group, [], TimePid).
    
%%**********************************************************
%% Receive messages to meeting group
%%**********************************************************
loop(Group,List, TimePid) ->
    List1 = receive 
        next_agenda -> 
            TimePid ! next_agenda, ok;
        {get_time_left, Pid} -> 
            TimePid ! {get_time_left, Pid}, ok;
        {msg, Map} -> 
            ets:insert(messages_group, {Group, [Map|List]}),
            [Map | List]
    end,
    loop(Group,List1, TimePid).
%%**********************************************************
%% Keep track of agenda
%%**********************************************************
time(Group, [{Time, _Entry}|T]) ->
    Reminder = spawn(?MODULE, reminder, [Group, Time]),
    StartTime = os:system_time(millisecond),
    receive
        {get_time_left, Pid} -> 
            A = os:system_time(millisecond) - StartTime,
            Pid ! (Time - A);
        next_agenda -> 
            exit(Reminder),
            time(Group, T)
    after Time ->
        [{_, _NextAgenda} | _] = T,
        ws_h:broadcast(Group, <<"Timeslot DONE!">>)
    end, time(Group, T).

reminder(Group, T) ->
    T1 = T - 6000,
    receive
    after T1 -> ws_h:broadcast(Group, <<"one min left">>)
    end.