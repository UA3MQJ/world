-module(wf_bilet2).

-export([start/0, stop/0, br/1, is_target/1, is_dead/1, ent/1, lib/0]).
-export([cnt/0]).

%%хеш мира
w2hash(Wrld) ->
    {_St_d,_En_d,X1,X2,X3,X4,X5,X6} = Wrld,
    erlang:phash2({X1,X2,X3,X4,X5,X6}).

%%кол-во результатов
cnt() -> cnt(0).

cnt(N) ->
    receive
    inc -> cnt(N+1);
    {cnt,Pid} ->
        Pid ! N,
        cnt(N)
    end.
	
%%хранилище миров
lib() -> lib([]).
    
lib(List) ->
    receive
    stop -> ok;
    {Wrld, Pid} ->
        WHash = w2hash(Wrld),
        NewList = case lists:member(WHash, List) of
                    false ->
                        Pid ! ok,
                        [WHash]++List;
                    true ->
                        Pid ! exist,
                        List
                  end,
        lib(NewList);
    _ -> ok
    end.
    
ent([]) -> ok;

%%на вход подается список кортежей миров, список делится на первый мир в списке Wrld
%%и на хвост Tail из оставшихся
ent([Wrld|Tail]) ->
    try spawn(?MODULE, ent, [Wrld]) of
        _Pid -> ent(Tail)
    catch
        _:_ -> 
        io:format("spawn overload~n", []),
        ent(Wrld),
        ent(Tail)
    end;

%%на вход подается кортеж одного мира
ent(Wrld) ->
    %lib_srv ! {Wrld, self()}, %% проверяем существует ли такой мир уже
	global:send("lib_srv", {Wrld, self()}),
    receive 
        ok ->
            is_target(Wrld), %% является ли целевым
            Is_dead = is_dead(Wrld), %% является ли тупиком
            if
                (Is_dead==false) -> %% если не тупик - плодим ветки и идем в них
                    NewBranches = br(Wrld),
                    ent(NewBranches);
                true -> ok
            end;
        exist ->
            ok
    end.
    
stop() ->
    %lib_srv ! stop.
	global:send("lib_srv", stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% функции, определяемые пользователем
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Мир достиг цели?
is_target(Wrld) ->
    {_St_d,_En_d,X1,X2,X3,X4,X5,X6} = Wrld,
    if ((X1+X2+X3)==(X4+X5+X6)) ->
        %cnt_srv ! inc,
        %cnt_srv ! {cnt, self()},
		global:send("cnt_srv", inc),
		global:send("cnt_srv", {cnt, self()}),
        receive
            X -> ok
        end,
        io:format("~w ~w ~w ~w ~w ~w (~w)~n", [X1,X2,X3,X4,X5,X6, X]);
        true -> ok
    end,
    ((X1+X2+X3)==(X4+X5+X6)).

%%Мир - тупиковая ветвь
is_dead(Wrld) ->
    {St_d,En_d,_X1,_X2,_X3,_X4,_X5,_X6} = Wrld,
    (St_d == En_d).

%%Ветвление мира
br(Wrld) ->
    {St_d,En_d,X1,X2,X3,X4,X5,X6} = Wrld,
    THalf = round((St_d + En_d) / 2),
    if
        (St_d == En_d) ->
            [];
        ((En_d - St_d) == 1) -> 
            XX6 = En_d rem 10,
            XX5 = trunc((En_d rem 100)/10),
            XX4 = trunc((En_d rem 1000)/100),
            XX3 = trunc((En_d rem 10000)/1000),
            XX2 = trunc((En_d rem 100000)/10000),
            XX1 = trunc((En_d rem 1000000)/100000),
            [{St_d,St_d,XX1,XX2,XX3,XX4,XX5,XX6}] ++
            [{En_d,En_d,XX1,XX2,XX3,XX4,XX5,XX6}];
        true ->
            br({St_d,THalf,X1,X2,X3,X4,X5,X6}) ++
            br({THalf,En_d,X1,X2,X3,X4,X5,X6})
    end.
    
%%запуск расчета
start() ->
    %register(lib_srv, spawn(?MODULE, lib, [])),
    %register(cnt_srv, spawn(?MODULE, cnt, [])),	
    PID_lib_srv = spawn(?MODULE, lib, []),
	global:register_name("lib_srv", PID_lib_srv),
    PID_cnt_srv = spawn(?MODULE, cnt, []),	
	global:register_name("cnt_srv", PID_cnt_srv),
    io:format("start~n", []),
    %% внутри [] нужно передать начальный мир
    spawn(?MODULE, ent, [{0,999999,0,0,0,0,0,0}]).