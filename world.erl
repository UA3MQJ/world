-module(world).
%world framework functions
-callback hash(Wrld :: term()) -> term().
-callback start() -> term().
-callback is_target(Wrld :: term()) -> term().
-callback is_dead(Wrld :: term()) -> term().
-callback br(Wrld :: term()) -> term().

-export([start/2, std_hash/1]).

-export([lib/1, lib/2, ent/2]).
%-export([srch/3]).
-export([is_overload/0]).

std_hash(Wrld)-> erlang:phash2(Wrld).

is_overload() ->
	PLimit = erlang:system_info(process_limit),
	PCount = erlang:system_info(process_count),
	GLimit = PLimit * 0.9,
	PCount > GLimit.

%хранилище миров - стандартное
%lib(Mod) -> lib(Mod, []).
% 
%lib(Mod, List) ->
%    receive
%		stop -> ok;
%		{Wrld, Pid} ->
%			WHash = Mod:hash(Wrld),
%			NewList = case lists:member(WHash, List) of
%						false ->
%							Pid ! ok,
%							[WHash]++List;
%						true ->
%							Pid ! exist,
%							List
%					  end,
%			lib(Mod, NewList);
%		_ -> ok
 %   end.

%%хранилище миров - ets с процессами (переделать под spawn overload)
%srch(Mod, Tbl, {Wrld, Pid}) ->
%	WHash = Mod:hash(Wrld),
%	case ets:member(Tbl, WHash) of
%		false ->
%			io:format("ets:insert(Tbl=~w, ~w)~n", [Tbl, WHash]),
%			ets:insert(Tbl, {WHash, ok}),
%			Pid ! ok;							
%		true ->
%			Pid ! exist
%   end.
%
%lib(Mod) -> 
%	Tbl = ets:new(wrld_list, []),
%	lib(Mod, Tbl).
% 
%lib(Mod, Tbl) ->
%    receive
%		stop -> ok;
%		{Wrld, Pid} ->
%			%without exception
%			%spawn(?MODULE, srch_proc, [Mod, Tbl, {Wrld, Pid}, self()]),
%			
%			case is_overload() of
%				false ->
%					try spawn(?MODULE, srch, [Mod, Tbl, {Wrld, Pid}]) of
%						_Pid -> ok
%					catch
%						_:_ -> 
%							io:format("world:lib spawn overload~n", []),
%							srch(Mod, Tbl, {Wrld, Pid})
%					end;
%				_ ->
%					%io:format("world:lib soft overload~n", []),
%					srch(Mod, Tbl, {Wrld, Pid})
%			end,
%			lib(Mod, Tbl);
%		_ -> ok
%	end.
	
%%хранилище миров - ets
lib(Mod) -> 
	Tbl = ets:new(wrld_list, []),
	lib(Mod, Tbl).
 
lib(Mod, Tbl) ->
    receive
		stop -> ok;
		{Wrld, Pid} ->
			WHash = Mod:hash(Wrld),
			case ets:member(Tbl, WHash) of
				false ->
					Pid ! ok,
					ets:insert(Tbl, {WHash});
				true ->
					Pid ! exist
			end,
			lib(Mod, Tbl);
		_ -> ok
    end.
	
%%на вход подается список кортежей миров, список делится на первый мир в списке Wrld
%%и на хвост Tail из оставшихся
ent(_Mod, []) ->
	%io:format("ent(_Mod, []) -> ok~n", []),
	ok;

ent(Mod, [Wrld|Tail]) ->
	%io:format("ent(~w, [~w|~w]) ->~n", [Mod, Wrld, Tail]),
	
	case is_overload() of
		false ->
			try spawn(?MODULE, ent, [Mod, Wrld]) of
			%%erlang pool
			%%try pool:pspawn(?MODULE, ent, [Mod, Wrld]) of
				_Pid -> ent(Mod, Tail)
			catch
				_:_ -> 
				io:format("world:ent spawn overload~n", []),
				ent(Mod, Wrld),
				ent(Mod, Tail)
			end;
		_ ->
			ent(Mod, Wrld),
			ent(Mod, Tail)
	end;
	

%%на вход подается кортеж одного мира
ent(Mod, Wrld) ->
	%io:format("ent(~w, ~w) ->~n", [Mod, Wrld]),
    %% проверяем существует ли такой мир уже
	global:send("lib_srv", {Wrld, self()}),
    receive 
        ok ->
			%io:format("world:ent World not exist~n", []),
            Mod:is_target(Wrld), %% является ли целевым
            Is_dead = Mod:is_dead(Wrld), %% является ли тупиком
            if
                (Is_dead==false) -> %% если не тупик - плодим ветки и идем в них
                    NewBranches = Mod:br(Wrld),
                    ent(Mod, NewBranches);
                true -> ok
            end;
        exist ->
			%io:format("world:ent World exist~n", []),
            ok
    end.

%%запуск расчета
start(Mod, StartWrld) ->
	io:format("world:start(~w,~w)~n", [Mod, StartWrld]),
    PID_lib_srv = spawn(?MODULE, lib, [Mod]),
	global:register_name("lib_srv", PID_lib_srv),
	io:format("world:start lib PID=~w~n", [PID_lib_srv]),
	PID_ent = spawn(?MODULE, ent, [Mod, StartWrld]),
	io:format("world:start ent PID=~w~n", [PID_ent]).
