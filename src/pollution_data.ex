defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(filename) do
    splitedLines = File.read!(filename)
                   |> String.split("\n")

    IO.puts("There are #{length(splitedLines)} lines")
    splitedLines
  end

def convertHead(line) do
    divided=String.split(line,";")
    [_,a | tail] = divided
    [_,type,_]=String.split(a,"-")
    type = String.to_charlist(type)
    tail = [a] ++ tail
    newtail = for x <- tail,
                  [name,_,coords]=String.split(x,"-"),
                  [xcord,ycord]=String.split(coords, ","),
                  {xcord,_} = Float.parse(xcord),
                  {ycord,_} = Float.parse(ycord),
                  name = String.to_charlist(name),
                  do:
                    {name,{xcord,ycord}}

    [type, newtail]
  end

  def convertLine(line) do
    divided=String.split(line,";")
    [head| tail] = divided
    [date, hour] = String.split(head, " ")
    date = String.split(date,"-")
           |> Enum.map(fn x -> {val,_}=Integer.parse(x); val end)
           |> :erlang.list_to_tuple()


    hour = String.split(hour, ":")
           |> Enum.map(fn x -> {val,_}=Integer.parse(x); val end)
    hour = hour ++ [0]
           |> :erlang.list_to_tuple()

    values = for x <- tail do
                            case String.length(x)>1 do
                              true -> {v,_} = Float.parse(x); v
                              false -> v=0; v end
                          end


    %{:datetime=>{date,hour}, :pollutionLevel=>values }
  end




  def loadStations(stations) do
#    :pollution_sup.start_link()
    stations
    |> Enum.each(fn {name, coords} ->
                             :pollution_gen_server.addStation(name, coords, :automat) end )
  end



def addValues5(values, coords, type) do
   Enum.each(values, fn(x) -> [a,b,c,d,e | _]=x.pollutionLevel;
                                       :pollution_gen_server.addValue(coords.a, x.datetime, type, a);
                                       :pollution_gen_server.addValue(coords.b, x.datetime, type, b);
                                       :pollution_gen_server.addValue(coords.c, x.datetime, type, c);
                                       :pollution_gen_server.addValue(coords.d, x.datetime, type, d);
                                       :pollution_gen_server.addValue(coords.e, x.datetime, type, e)
                    end)
  end

  def addValues4(values, coords, type) do
    Enum.each(values, fn(x) -> [a,b,c,d  | _]=x.pollutionLevel;
                               :pollution_gen_server.addValue(coords.a, x.datetime, type, a);
                               :pollution_gen_server.addValue(coords.b, x.datetime, type, b);
                               :pollution_gen_server.addValue(coords.c, x.datetime, type, c);
                               :pollution_gen_server.addValue(coords.d, x.datetime, type, d);
    end)
  end

  def addValues2(values, coords, type) do
    Enum.each(values, fn(x) -> [a,b  | _]=x.pollutionLevel;
                               :pollution_gen_server.addValue(coords.a, x.datetime, type, a);
                               :pollution_gen_server.addValue(coords.b, x.datetime, type, b);

    end)
  end


  def importData() do
    data =
      "src/gios-pjp-data-tlenkiazotu.csv"
    |> importLinesFromCSV()

    [h|t] = data
    [type, list] = convertHead(h)
    loadStations(list)

    [a,b,c,d] = for x <- list, {_,cor}=x, do: cor
    coords = %{:a=>a, :b=>b, :c=>c, :d=>d}

    values = t
    |> Enum.map(&convertLine/1)
    addValues4(values,coords,type)

#    =================
    data =
      "src/gios-pjp-data-pm10.csv"
      |> importLinesFromCSV()

    [h|t] = data
    [type, list] = convertHead(h)
    loadStations(list)

    [a,b,c,d,e] = for x <- list, {_,cor}=x, do: cor
    coords = %{:a=>a, :b=>b, :c=>c, :d=>d, :e=>e}

    values = t
             |> Enum.map(&convertLine/1)
    addValues5(values,coords,type)

#===============
    data =
      "src/gios-pjp-data-pm25.csv"
      |> importLinesFromCSV()

    [h|t] = data
    [type, list] = convertHead(h)
    loadStations(list)

    [a,b] = for x <- list, {_,cor}=x, do: cor
    coords = %{:a=>a, :b=>b}

    values = t
             |> Enum.map(&convertLine/1)
    addValues2(values,coords,type)



  end
#    IO.puts("\nloadStations time #{measureTime(fn -> loadStations(stations) end) }" )
#    IO.puts("loadStationsData time #{measureTime(fn -> loadStationsData(data) end) }")
#
#    IO.puts("StationMean time #{measureTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end)}")
#    IO.puts("DailyMean time #{measureTime(fn -> :pollution_gen_server.getDailyMean({2017, 5, 4}, "PM10") end)}")



end
#    c("pollution.erl")
#    c("pollution_sup.erl")
#    c("pollution_gen_server.erl")