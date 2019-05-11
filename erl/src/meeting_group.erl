%%**********************************************************
%% TODO:
%% Add meeting host
%%**********************************************************

-module(meeting_group).

-export([start/5]).
-export([init/2]).
-export([time/2]).
-export([time/3]).
-export([reminder/3]).
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
start(Group, Name, Creator, Purpose, Agenda) ->
	pg2:create(Group),
    register(
        binary_to_atom(Group, latin1), 
        spawn_link(?MODULE, init, [Group, Agenda])
    ),
    ets:insert(group, {{Group, <<"name">>}, Name}),
    ets:insert(group, {{Group, <<"creator">>}, Creator}),
    ets:insert(group, {{Group, <<"purpose">>}, Purpose}),
    ets:insert(group, {{Group, <<"agenda">>}, Agenda}),
    ets:insert(group, {{Group, <<"messages">>}, []}),
    ets:insert(
        group, {{Group, <<"status">>}, <<"active">>}
    ).

% -spec init(Group, Agenda) -> no_return().

init(Group, Agenda) -> 
	Agenda1 = lists:map(fun(Entry) -> 
            {
                maps:get(<<"to">>, Entry) - maps:get(<<"from">>, Entry),
                maps:get(<<"title">>, Entry)
            }
        end, Agenda),
    TimePid = spawn_link(?MODULE, time, [start, Group, Agenda1]),
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
        start_meeting -> 
            TimePid ! start_meeting,
            loop(Group, List, TimePid, PrologPid);
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
                {{Group, messages}, List1}
            ),
            loop(Group, List1, TimePid, PrologPid)
    end.

%%**********************************************************
%% Keep track of agenda
%%**********************************************************
reminder(_, _, die) ->ok;
reminder(Group, StartTime, _) ->
    D = receive _-> die
    after 1000 ->  
        X = os:system_time(millisecond) - StartTime,
        M = jsone:encode(#{
            <<"time">> => X
        }),
        ws_h:broadcast(Group, M),
        ok
    end,
    reminder(Group, StartTime, D).

time(start, Group, List) ->
    receive
        start_meeting -> ok
    end, 
    time(Group, List).

time(Group, []) ->
    receive _ -> ok end, 
    time(Group, []);

time(Group, [{Time, _Entry}|T]) ->
    
    StartTime = os:system_time(millisecond),
    % {ok, Timer} = timer:apply_interval(1000, ?MODULE, reminder, [Group, StartTime]),
    Timer = spawn(?MODULE, reminder, [Group, StartTime, ok]),
    receive
        next_agenda -> ok;
            % exit(Timer, poop);
        _ -> ok
    after Time ->
        case T of
            [{_, _NextAgenda} | _] -> 
                    ws_h:broadcast(Group, <<"Timeslot DONE!">>);
            _ -> ok
        end
    end, 
    exit(Timer, poop),
    time(Group, T).
%%**********************************************************
%% 
%%**********************************************************
% reminder(_Group, T) ->
%     T1 = T - 6000,
%     receive
%         die ->
%             io:format("iam now dead!"),
%             ok
%     after T1 -> io:format("dying after 6sec")%ws_h:broadcast(Group, <<"one min left">>)
%     end.

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
