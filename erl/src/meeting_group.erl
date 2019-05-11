%%**********************************************************
%% TODO:
%% Add meeting host
%%**********************************************************

-module(meeting_group).

-export([start/4]).
-export([init/2]).
-export([time/2]).
-export([reminder/2]).
-export([users/2]).
% -export([prolog/1]).
% -export([prolog_loop/1]).

%%**********************************************************
%% Start a mailbox for each new group. This mailbox starts 
%% other processes, keeping track of e.g. time and group 
%% members.
%% meeting:start("group1", Name, Creator, Agenda).
%%**********************************************************
% -spec start(Group, Agenda) -> no_return().
start(Group, Name, Creator, Agenda) ->
	pg2:create(Group),
    register(
        binary_to_atom(Group, latin1), 
        spawn_link(?MODULE, init, [Group, Agenda])
    ),
    ets:insert(group, {{Group, <<"name">>}, Name}),
    ets:insert(group, {{Group, <<"creator">>}, Creator}),
    ets:insert(group, {{Group, <<"agenda">>}, Agenda}),
    ets:insert(group, {{Group, <<"messages">>}, []}),
    ets:insert(
        group, {{Group, <<"status">>}, <<"active">>}
    ).

% -spec init(Group, Agenda) -> no_return().
init(Group, _Agenda) -> 
    % PrologPid = spawn(?MODULE, prolog, [Group]),
	% _Agenda1 = lists:map(fun(Entry) -> 
    %         {
    %             maps:get(<<"time">>, Entry),
    %             maps:get(<<"title">>, Entry)
    %         }
    %     end, Agenda),
    % TimePid = spawn_link(?MODULE, time, [Group, Agenda1]),
    TimePid = null,
    UserPid = spawn_link(?MODULE, users, [Group, 0]),
    timer:send_interval(
        1000 * 60, 
        UserPid, 
        length(lists:usort(pg2:get_members(Group)))
    ),
    PrologPid = null,
    loop(Group, [], TimePid, PrologPid).
    
%%**********************************************************
%% Receive messages to meeting group
%%**********************************************************
loop(Group, List, TimePid, PrologPid) ->
    % PrologPid ! hello,
    receive 
        next_agenda -> 
            TimePid ! next_agenda,
            loop(Group, List, TimePid, PrologPid);
        {get_time_left, Pid} -> 
            TimePid ! {get_time_left, Pid},
            loop(Group, List, TimePid, PrologPid);
        {msg, Map} -> 
            List1 = [Map|List],
            ets:insert(
                group, 
                {{Group, messages}, jsone:encode(List1)}
            ),
            loop(Group, List1, TimePid, PrologPid)
    end.

%%**********************************************************
%% Keep track of agenda
%%**********************************************************
time(_Group, []) ->
    io:format("meeting done!");
time(Group, [{Time, _Entry}|T]) ->
    Reminder = spawn(?MODULE, reminder, [Group, Time]),
    StartTime = os:system_time(millisecond),
    receive
        {get_time_left, Pid} -> 
            A = os:system_time(millisecond) - StartTime,
            Pid ! (Time - A);
        next_agenda -> 
            Reminder ! die;
        _ -> ok
    after Time ->
        case T of
            [{_, _NextAgenda} | _] -> 
                    ws_h:broadcast(Group, <<"Timeslot DONE!">>);
            _ -> ok
        end
    end, 
    time(Group, T).

%%**********************************************************
%% 
%%**********************************************************
reminder(_Group, T) ->
    T1 = T - 6000,
    receive
        die ->
            io:format("iam now dead!"),
            ok
    after T1 -> io:format("dying after 6sec")%ws_h:broadcast(Group, <<"one min left">>)
    end.

%%**********************************************************
%% 
%%**********************************************************
users(Group, N) -> 
    receive
        0 when N > 1000 * 60 * 20 -> 
            ets:insert(group, {{Group, <<"users">>}, 0}),
            ets:insert(group, 
                {{Group, <<"status">>}, <<"finished">>}),
            exit(binary_to_atom(Group, latin1), abandoned);
        0 -> users(Group, N + 1);
        N1 -> 
            ws_h:broadcast(
                Group, jsone:encode(#{<<"users">> => N1})
            ),
            ets:insert(group, {{Group, <<"users">>}, N}),
            users(Group, 0)
    end.

%%**********************************************************
%%
%%**********************************************************
% prolog(Group) ->
%     {ok, ConnPid} = gun:open("localhost", 8000),
%     gun:ws_upgrade(ConnPid, "/ws"),
%     spawn(?MODULE, prolog_loop, [Group]),
%     receive
%         X -> io:format("X ~p~n",[X])
%     end.        

% prolog_loop(Group) ->
%     receive
%         {gun_response, _, _, _, _, _} -> 
%             prolog(Group);
%         {gun_error, _, _, _} ->
%             prolog(Group);
%         {gun_data,_ ,_ , _, Data} ->
%             io:format("Data ~p~n",[Data]);
%         _ -> prolog_loop(Group)
%     end.        
