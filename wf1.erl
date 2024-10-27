-module(wf1).

-export([start/0, stop/0, br/1, is_target/1, is_dead/1, ent/1, lib/0]).

%%хеш мира
w2hash(Wrld) ->
    erlang:phash2(Wrld).

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
    lib_srv ! {Wrld, self()}, %% проверяем существует ли такой мир уже
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
    lib_srv ! stop.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% функции, определяемые пользователем
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Мир достиг цели?
is_target(Wrld) ->
    true.

%%Мир - тупиковая ветвь
is_dead(Wrld) ->
    false.

%%Ветвление мира
br(Wrld) ->
[].
    
%%запуск расчета
start() ->
    register(lib_srv, spawn(?MODULE, lib, [])),
    io:format("start~n", []),
    %% внутри [] нужно передать начальный мир
    spawn(?MODULE, ent, [[]]).