%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2020 04:57
%%%-------------------------------------------------------------------
-module(pollution).
-author("Natalia Brzozowska").

%% API
-export([startMonitor/1, getAllStations/0,stopMonitor/1,addStation/3,addValue/4,removeValue/3,getOneValue/3, getStationMean/2,
  getDailyMean/2, getWorstDay/2,compareMeasurements/3,getWorstHourlyStation/3,
  compareMeasurementsHourly/3, removeStation/1, getAllHourMeasurements/2, getStationAllMeasurements/1, getStationAttributes/1]).


%%-----------------------data structure---------------------------
%%All data is in database

%%-----------------------functions---------------------------------
%%-----------createMonitor----------
%% 'createMonitor' is a function which starts database.


startMonitor(Nodes) -> mnesia_database:startDB(Nodes).


stopMonitor(Nodes) -> mnesia_database:stopDB(Nodes).


addStation( Name, Coordinates, Type)  ->
  case mnesia_database:addStationDB(Name,Coordinates, Type) of
    ok -> {reply,'station added'};
    A -> A
  end.



addValue(Id, Date, Type, Value ) ->
 case mnesia_database:addMeasurementDB(Id,Date,Type,Value) of
   ok -> {reply, 'value added'};
   A -> A
 end.


removeValue(Id, Date, Type) ->
  case mnesia_database:deleteMeasurementDB(Id, Date, Type) of
    ok -> {reply,'value removed'};
    A  -> A
  end.



getOneValue(Id, Date, Type) ->
  case mnesia_database:findMeasurementsDB(one, [Id, Date, Type]) of
    [{_,_,_,_,Val}] ->  {reply,Val};
    A -> A
  end.


getStationMean(Id, Type) ->
  Measurements = mnesia_database:findMeasurementsDB(type, [Id,Type]),
  case is_tuple(Measurements) of
    true -> {reply,0.0};
    false -> Values = lists:map(fun({_,_,_,_,Val}) -> Val end, Measurements),
              {reply,lists:foldl(fun(X, Sum) -> X + Sum end, 0, Values) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, Values)}
  end.




getDailyMean(Day,Type) ->
  Measurements = mnesia_database:findMeasurementsDB(date, Day),
  case is_tuple(Measurements) of
    true -> {reply,0.0};
    false ->  TypeValues = lists:filter(fun
                                          ({_,_,MeasurementType,_,_}) when MeasurementType==Type -> true;
                                          (_) -> false
                                        end, Measurements),
      Values = lists:map(fun({_,_,_,_,Val}) -> Val end, TypeValues),
      No = lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, Values),
      {reply,lists:foldl(fun(X, Sum) -> X + Sum end, 0, Values) / No}
  end.


getWorstDay(Id, Type) ->
  Measurements = mnesia_database:findMeasurementsDB(type, [Id, Type]),
  case is_tuple(Measurements) of
    true ->  Measurements;
    false ->  Values = lists:map(fun ({_,_,_,Date,Val}) -> {Date, Val} end, Measurements),
      compareMeasurements(Values,{}, 0 )
  end.




compareMeasurements([],WorstDate, WorstVal) ->
  {reply,{WorstDate, WorstVal}};
compareMeasurements([{Date, Value} | Tail],WorstDate, WorstVal) ->
  case Value > WorstVal of
    true -> compareMeasurements(Tail,Date, Value);
    false -> compareMeasurements(Tail,WorstDate, WorstVal)
  end.


getWorstHourlyStation( Day, Hour, Type) ->
  Measurements = mnesia_database:findMeasurementsDB(date, [Day, Hour]),
  TypeMeasurements = lists:filter(fun ({_,_,MType,_,_}) when MType == Type -> true;
                                     (_) -> false end, Measurements),
  Values = lists:map(fun ({_,Id,_,_,Val}) -> {Id, Val} end, TypeMeasurements),
  compareMeasurementsHourly(Values,0,0 ).


compareMeasurementsHourly([],WorstStation,WorstValue) ->
  {reply,{WorstStation, WorstValue}};
compareMeasurementsHourly([{Id,Val} | Tail],WorstStationName,WorstValue) ->
  case Val >= WorstValue of
    true -> compareMeasurementsHourly(Tail,Id, Val);
    false -> compareMeasurementsHourly(Tail,WorstStationName, WorstValue)
  end.


removeStation(Id) ->
  case mnesia_database:deleteStationDB(Id) of
    ok -> {reply,'station removed'};
    A  -> A
  end.

getStationAttributes(Id) ->
  B = mnesia_database:findStationDB(Id),
  case is_tuple(B) of
    false -> [Tup]=B, {reply,Tup};
    true  -> B
  end.


getStationAllMeasurements(Id) ->
  M = mnesia_database:findMeasurementsDB(id, Id),
  case is_tuple(M) of
    true -> M;
    false -> {reply, M}
  end.

getAllHourMeasurements(Day, Hour) ->
  M = mnesia_database:findMeasurementsDB(date, [Day, Hour]),
  case is_tuple(M) of
  true -> M;
  false -> {reply, M}
  end.


getAllStations() ->
  M = mnesia_database:findAllStations(),
  case is_tuple(M) of
    true -> M;
    false -> {reply, M}
  end.