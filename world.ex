defmodule World do
  @callback hash(wrld :: term()) :: term()
  @callback start() :: term()
  @callback is_target(wrld :: term()) :: term()
  @callback is_dead(wrld :: term()) :: term()
  @callback br(wrld :: term()) :: term()

  require Logger

  def is_overload() do
    p_limit = :erlang.system_info(:process_limit)
    p_count = :erlang.system_info(:process_count)
    g_limit = p_limit * 0.9
    p_count > g_limit
  end

  def std_hash(world),
    do: :erlang.phash2(world)

  # хранилище миров - стандартное
  # def lib(mod), do: lib(mod, [])
  # def lib(mod, list) do
  #   receive do
  #     :stop -> :ok
  #     {wrld, pid} ->
  #       w_hash = apply(mod, :hash, [wrld])
  #       new_list = case :lists.member(w_hash, list) do
  #         false ->
  #           send(pid, :ok)
  #           [w_hash] ++ list
  #         true ->
  #           send(pid, :exist)
  #           list
  #       end
  #       lib(mod, new_list)
  #     _ -> :ok
  #   end
  # end

  # хранилище миров - ets
  def lib(mod) do
    tbl = :ets.new(:wrld_list, [])
    lib(mod, tbl)
  end
  def lib(mod, tbl) do
    receive do
      :stop -> :ok
      {wrld, pid} ->
        w_hash = apply(mod, :hash, [wrld])
        case :ets.member(tbl, w_hash) do
          false ->
            send(pid, :ok)
            :ets.insert(tbl, {w_hash})
          true ->
            send(pid, :exist)
        end
        lib(mod, tbl)
      _ -> :ok
    end
  end

  # на вход подается список кортежей миров, список делится на первый мир в списке Wrld
  # и на хвост Tail из оставшихся
  def ent(_mod, []), do: :ok

  def ent(mod, [wrld|tail]) do
    case is_overload() do
      false ->
        try do
            spawn(__MODULE__, :ent, [mod, wrld])
            ent(mod, tail)
        catch
          _ ->
            Logger.error("world:ent spawn overload")
            ent(mod, wrld)
            ent(mod, tail)
        end
      _ ->
        ent(mod, wrld)
        ent(mod, tail)
    end
  end

  # на вход подается кортеж одного мира
  def ent(mod, wrld) do
    # проверяем существует ли такой мир уже
    :global.send('lib_srv', {wrld, self()})
    receive do
      :ok ->
        # является ли целевым
        apply(mod, :is_target, [wrld])
        # является ли тупиком
        is_dead = apply(mod, :is_dead, [wrld])
        # если не тупик - плодим ветки и идем в них
        if not(is_dead) do
          new_branches = apply(mod, :br, [wrld])
          ent(mod, new_branches)
        else
          :ok
        end
      :exist ->
        :ok
    end
  end

  def start(mod, start_world) do
    Logger.info("World.start mod=#{inspect(mod)}")
    Logger.info("World.start start_world=#{inspect(start_world)}")
    pid_lib_srv = spawn(__MODULE__, :lib, [mod])
    :global.re_register_name('lib_srv', pid_lib_srv)
    Logger.info("World.start lib(worldlist) PID=#{inspect(pid_lib_srv)}")
    pid_lib_srv = spawn(__MODULE__, :ent, [mod, start_world])
    Logger.info("World.start ent PID=#{inspect(pid_lib_srv)}")
  end
end
