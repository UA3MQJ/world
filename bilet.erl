-module(bilet).
-behaviour(world).
-export([start/0, hash/1, br/1, is_dead/1, is_target/1 ]).

-export([cnt/0]).

%%кол-во найденных результатов
cnt() -> cnt(0).

cnt(N) ->
    receive
    inc -> cnt(N+1);
    {cnt,Pid} ->
        Pid ! N,
        cnt(N)
    end.

%%ХЭШ мира. используем стандартный
hash(Wrld) ->
	world:std_hash(Wrld).
	
%%Мир достиг цели?
is_target(Wrld) ->
    {_St_d,_En_d,X1,X2,X3,X4,X5,X6} = Wrld,
    % if ((X1+X2+X3)==(X4+X5+X6)) ->
	% 	global:send("cnt_srv", inc),
	% 	global:send("cnt_srv", {cnt, self()}),
    %     receive
    %         X -> ok
    %     end,
    %     io:format("~w ~w ~w ~w ~w ~w (~w)~n", [X1,X2,X3,X4,X5,X6, X]);
    %     true -> ok
    % end,
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

start() ->
	io:format("start~n", []),
    PID_cnt_srv = spawn(?MODULE, cnt, []),	
	global:register_name("cnt_srv", PID_cnt_srv),
	
	world:start(?MODULE,[{0,999999,0,0,0,0,0,0}]),
	ok.