defmodule PollutionData do
  @moduledoc false

  def loadModules() do
    IEx.Helpers.c("../pollution/pollution.erl")
    IEx.Helpers.c("../pollution_gen_supervisor.erl")
    IEx.Helpers.c("../pollution_gen_server.erl")
  end

  def importLinesFromCSV() do
    fil = File.read!("pollution.csv") |> String.split()
    Enum.map(fil, fn(x)->  splitToTuple(x) end)

  end

  def splitToTuple(line) do
    [date, time, locationX, locationY, pollutionLevel]=String.split(line,",")
    date = String.split(date,"-") |> Enum.reverse |> Enum.map(fn x -> String.to_integer(x) end) |> List.to_tuple
    time = String.split(time,":") |> Enum.map(fn x -> String.to_integer(x) end) |> (fn(x)-> x++[0] end).() |> List.to_tuple
    location = {locationX |> String.to_float, locationY |> String.to_float}
    pollutionLevel = pollutionLevel |> String.to_integer
    %{:datetime => {date, time}, :location  => location, :pollutionLevel  => pollutionLevel}
    #    time = String.split()

  end

  def identifyStations(list) do
    loadModules()

    map = Enum.reduce(list, %{}, fn reading, acc -> Map.put(acc, reading.location, "station_#{elem(reading.location,0)}_#{elem(reading.location,1)}" ) end)
    :pollution_gen_supervisor.start()
    Enum.map( fn x -> end)
    :pollution_gen_server.addStation()

  end

end
