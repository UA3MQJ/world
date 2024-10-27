defmodule Bilet do
  require Logger
  @behaviour World

  # хранилище найденных результатов
  def cnt(), do: cnt(%{})
  def cnt(results) do
    receive do
      :stop -> :ok
      {{:add, result}, pid} ->
        new_results = Map.merge(results, %{result => :ok})
        l1 = length(Map.keys(new_results))
        l2 = length(Map.keys(results))
        if (l1 != l2) do
          Logger.info("Target #{inspect(result)}")
        end
        # send(pid, length(Map.keys(new_results)))
        cnt(new_results)
      _ -> :ok
    end
  end

  # ХЭШ мира. используем стандартный
  def hash(wrld), do:
    World.std_hash(wrld)

  # Мир достиг цели?
  def is_target({_st_d,_en_d,x1,x2,x3,x4,x5,x6} = _wrld) do
    if ((x1+x2+x3)==(x4+x5+x6)) do
      # Logger.info("Target #{inspect({x1,x2,x3,x4,x5,x6})}")
      :global.send('res_srv', {{:add, {x1,x2,x3,x4,x5,x6}}, self()})
      # receive do
      #   xxx ->
      #     Logger.info("Target #{inspect({x1,x2,x3,x4,x5,x6})} #{xxx}")
      # end

    end
    ((x1+x2+x3)==(x4+x5+x6))
  end

  # Мир - тупиковая ветвь
  def is_dead({st_d,en_d,_x1,_x2,_x3,_x4,_x5,_x6} = _wrld) do
    st_d == en_d
  end

  # Ветвление мира
  def br({st_d,en_d,x1,x2,x3,x4,x5,x6} = _wrld) do
    # Logger.info("br #{inspect({st_d,en_d})}")
    t_half = round((st_d + en_d) / 2)
    cond do
      (st_d == en_d) -> []
      ((en_d - st_d) == 1) ->
        xx6 = rem(en_d, 10)
        xx5 = trunc(rem(en_d, 100)/10)
        xx4 = trunc(rem(en_d, 1000)/100)
        xx3 = trunc(rem(en_d, 10000)/1000)
        xx2 = trunc(rem(en_d, 100000)/10000)
        xx1 = trunc(rem(en_d, 1000000)/100000)

        [{st_d, st_d, xx1,xx2,xx3,xx4,xx5,xx6}] ++
        [{en_d, en_d, xx1,xx2,xx3,xx4,xx5,xx6}]
      true ->
        br({st_d, t_half, x1,x2,x3,x4,x5,x6}) ++
        br({t_half, en_d, x1,x2,x3,x4,x5,x6})
    end
  end

  def start() do
    pid_res_srv = spawn(__MODULE__, :cnt, [])
    :global.re_register_name('res_srv', pid_res_srv)
    Logger.info("World.start res PID=#{inspect(pid_res_srv)}")

    World.start(__MODULE__, [{0,999999, 0,0,0,0,0,0}])
    :ok
  end
end
