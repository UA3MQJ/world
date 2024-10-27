defmodule Bilet2 do
  require Logger
  @behaviour World2

  # хранилище найденных результатов
  def cnt?() do
    :ets.info(:result_list)
  end

  # ХЭШ мира. используем стандартный
  def hash(wrld), do:
    wrld
    # World.std_hash(wrld) # почему-то при hash проёбываются 55062 вместо 55252

  # Мир достиг цели?
  def is_target({_,x1,x2,x3,x4,x5,x6} = _wrld) do
    res = ((x1+x2+x3)==(x4+x5+x6))
    if res do
      :ets.insert(:result_list, {{x1,x2,x3,x4,x5,x6}})
    end

    res
  end

  # Мир - тупиковая ветвь
  def is_dead({x,_x1,_x2,_x3,_x4,_x5,_x6} = _wrld) do
    x == 1
  end

  # Ветвление мира
  def br({1,_x1,_x2,_x3,_x4,_x5,_x6} = _wrld) do
    []
  end

  def br({0,_x1,_x2,_x3,_x4,_x5,_x6} = _wrld) do
    li = [0,1,2,3,4,5,6,7,8,9]
    for a<-li, b<-li, c<-li, d<-li, e<-li, f<-li,
      do: {1, a, b, c, d, e, f}
  end

  def start() do
    :ets.new(:result_list, [:named_table, :public, write_concurrency: true])

    World2.start(__MODULE__, [{0, 0,0,0,0,0,0}])
    :ok
  end
end
