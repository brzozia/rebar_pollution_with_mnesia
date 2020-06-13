-module(mnesia_database).
-author("Natalia Brzozowska").

%% API
-export([stopDB/1,startDB/1, findAllStations/0, addMeasurementDB/4,addStationDB/3,findStationDB/1, findMeasurementsDB/2, deleteStationDB/1, deleteMeasurementDB/3]).

-record(stations, {coordinates, name, creation_date, type}).
-record(measurements, {coordinates, type, date, value}).


startDB(Nodes) ->
  case mnesia:create_schema(Nodes) of
    ok ->  rpc:multicall(Nodes, application, start, [mnesia]),
            mnesia:create_table(stations, [           % table for stations
              {attributes, record_info(fields, stations)},
              {disc_copies, Nodes},
              {index, [#stations.name]},
              {type, set}]),
            mnesia:create_table(measurements, [       % table for measurements
              {attributes, record_info(fields, measurements)},
              {disc_copies, Nodes },
              {index, [#measurements.date]},
              {type,bag}]),
            rpc:multicall(Nodes, application, stop, [mnesia]);
  _ -> rpc:multicall(Nodes, application, start, [mnesia])
  end.


stopDB(Nodes) ->
  rpc:multicall(Nodes, application, stop, [mnesia]).
%%  mnesia:stop().


addStationDB(Name, Coordinates, Type) ->
  Fun = fun() -> case is_list(findStationDB(Coordinates)) orelse
                        is_list(findStationDB(Name))
                 of
                   true -> {error, same_station_attributes};
                   false ->  mnesia:write(#stations{coordinates = Coordinates,
                                                   name = Name,
                                                   creation_date = calendar:local_time(),
                                                   type = Type})
                 end
  end,
  mnesia:activity(transaction, Fun).


addMeasurementDB(Id, Date, Type, Value) when is_tuple(Id)->
  {Day,{Hour,_,_}}=Date,
  Fun = fun() -> case mnesia:read({stations, Id}) of
                   [] -> {error, wrong_station_id};
                   _  -> case mnesia:match_object({measurements, Id, Type,{Day, {Hour,'_', '_'}} ,'_'}) of
                                 [] -> mnesia:write(#measurements{coordinates = Id,
                                                                  date = Date,
                                                                  type = Type,
                                                                  value = Value});
                                  _ -> {error, same_values_to_station}
                               end
                 end
    end,
  mnesia:activity(transaction, Fun);
addMeasurementDB(Id, Date, Type, Value) ->
  case findStationDB(Id) of
    [{stations, Coords, _,_,_}] -> addMeasurementDB(Coords, Date, Type, Value);
    A -> A
  end.



deleteStationDB(Id) when is_tuple(Id) ->
  Fun = fun() -> case mnesia:read(stations,Id )  of
                   [] -> {error, wrong_station_id};
                   _ ->mnesia:delete({stations, Id}), mnesia:delete({measurements, Id})
                 end
        end,
  mnesia:activity(transaction, Fun);
deleteStationDB(Id) ->
  case findStationDB(Id) of
    [{stations, Coords, _,_,_}] -> deleteStationDB(Coords);
    A -> A
  end.


deleteMeasurementDB(Id, Date, Type) when is_tuple(Id) ->
  Fun = fun() -> case findMeasurementsDB(one, [Id,Date,Type]) of
                   [] -> {error, no_such_value};
                   [A] -> mnesia:delete_object(A);
                    B -> B
                 end
        end,
  mnesia:activity(transaction, Fun);
deleteMeasurementDB(Id, Date, Type) ->
  case findStationDB(Id) of
    [{stations, Coords, _,_,_}] ->   deleteMeasurementDB(Coords,Date,Type);
    A -> A
  end.


findAllStations()->
  Fun = fun () -> mnesia:match_object({stations,'_','_','_','_'}) end,
  mnesia:activity(transaction, Fun).

findStationDB(Key) when is_tuple(Key)->
  Fun = fun() -> case mnesia:read({stations,Key}) of
                   [] -> {error, wrong_station_id};
                   A -> A
                   end
        end,
  mnesia:activity(transaction, Fun);

findStationDB(Key) ->
  Pattern = {stations,'_',Key,'_','_'},
  Fun = fun() -> case mnesia:match_object(Pattern) of
                   [] -> {error, wrong_station_name};
                   A -> A
                 end
        end,
  mnesia:activity(transaction, Fun).



findMeasurementsDB(id, Key) when is_tuple(Key) ->
  Fun = fun() -> mnesia:read({measurements,Key}) end,
  mnesia:activity(transaction, Fun);

findMeasurementsDB(id, Key) ->
  case findStationDB(Key) of
    [{stations, Coords, _,_,_}] ->  findMeasurementsDB(id, Coords);
    A -> A
  end;

findMeasurementsDB(date, [Day, Hour]) ->
  runFunMatch({measurements, '_', '_',{Day, {Hour,'_', '_'}} ,'_'});

findMeasurementsDB(date, Day) ->
  runFunMatch({measurements, '_', '_',{Day, '_'} ,'_'});

findMeasurementsDB(one, [Id, Date, Type]) when is_tuple(Id)->
  runFunMatch( {measurements, Id, Type, Date ,'_'});

findMeasurementsDB(one, [Id, Date, Type]) ->
  case findStationDB(Id) of
    [{stations, Coords, _,_,_}] ->  runFunMatch( {measurements, Coords, Type, Date ,'_'});
    A -> A
  end;

findMeasurementsDB(type, [Id, Type]) when is_tuple(Id) ->
  runFunMatch({measurements, Id, Type, '_' ,'_'});

findMeasurementsDB(type, [Id, Type])  ->
  case findStationDB(Id) of
    [{stations, Coords, _,_,_}] ->  runFunMatch({measurements, Coords, Type, '_' ,'_'});
    A -> A
  end.


runFunMatch(Pattern) ->
  Fun = fun() -> case mnesia:match_object(Pattern) of
                   [] -> {error, no_such_value};
                   A -> A
                 end
    end,
  mnesia:activity(transaction, Fun).