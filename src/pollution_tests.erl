%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 20:18
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Natalia Brzozowska").

-include_lib("eunit/include/eunit.hrl").


addStation_test() ->
  pollution:startMonitor(),
  {reply,'station added'}= pollution:addStation("Krak",{1123.234,213.23}, type),

  ?assertEqual({error,same_station_attributes},pollution:addStation("Bronowice",{1123.234,213.23}, type) ), %% same coordinates as before
  ?assertEqual({error,same_station_attributes}, pollution:addStation("Krak",{187.234,29.23}, type) ),   %% same name as before
  pollution:removeStation("Krak"),
  pollution:stopMonitor().


addValueGetOneValueRemoveStation_test() ->
  pollution:startMonitor(),

  ?assertEqual({reply,'station added'},pollution:addStation( "Bronowice", {31.23,45.67}, type)),
  ?assertEqual({reply,'value added'}, pollution:addValue({31.23,45.67},  {{2020,04,21},{11,11,11}}, "PM2.5", 40.4)),
  ?assertEqual({reply,'value added'}, pollution:addValue( "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15)),

  ?assertEqual({error,same_values_to_station},pollution:addValue( "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15) ), %% same values as up
  ?assertEqual({reply,'value added'},pollution:addValue( "Bronowice",  {{2020,04,21},{12,11,12}}, "PM2.5", 40.4) ), %% returns new monitor - tuple
  ?assertEqual({error,wrong_station_name},pollution:addValue("Zabierzow",  {{2020,04,21},{11,11,13}}, "Temp", 15) ), %% gets value from station that does not exist

  ?assertEqual({reply,40.4},pollution:getOneValue("Bronowice", {{2020,04,21},{11,11,11}}, "PM2.5")), %% getOneValue test
  ?assertEqual({error,wrong_station_name},pollution:getOneValue("Zabierzow", {{2020,04,21},{11,11,11}}, "PM2.5")), %% wrong name
  ?assertEqual({error,no_such_value},pollution:getOneValue("Bronowice", {{2020,04,21},{11,11,11}}, "Temp")), %% wrong date

  ?assertEqual({reply, 'station removed'},pollution:removeStation("Bronowice")),
  pollution:stopMonitor().


removeValueAndOther_test() ->
  pollution:startMonitor(),

  ?assertEqual({reply,'station added'}, pollution:addStation("Bronowice", {31.23,45.67}, test)),
  ?assertEqual({reply,'value added'}, pollution:addValue( {31.23,45.67},  {{2020,04,21},{11,11,11}}, "PM2.5", 40.4)),
  ?assertEqual({reply,'value added'}, pollution:addValue( "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp", 15)),
  ?assertEqual({reply,'value removed'}, pollution:removeValue("Bronowice",{{2020,04,21},{11,11,11}}, "PM2.5")),

  ?assertEqual({error,no_such_value},pollution:getOneValue("Bronowice",{{2020,04,21},{11,11,11}}, "PM2.5")), %% checks deleted value
  ?assert(is_tuple(pollution:removeValue( "Bronowice",  {{2020,04,21},{11,11,13}}, "Temp") )), %% remove returns new monitor - tuple
  ?assertEqual({error,no_such_value},pollution:removeValue( "Bronowice",  {{2020,04,21},{11,11,12}}, "PM2.5")), %% tries to remove this same value again - error occurs

  ?assertEqual({reply, 'station removed'},pollution:removeStation("Bronowice")),
  pollution:stopMonitor().

getOneValueGetMeanAndOther_test() ->
  pollution:startMonitor(),

  ?assertEqual({reply,'station added'},pollution:addStation( "Bronowice", {31.23,45.67}, test)),
  ?assertEqual({reply,'value added'},pollution:addValue( {31.23,45.67}, {{2020,04,21},{11,11,11}}, "PM10", 23.4)),
  ?assertEqual({reply,'value added'}, pollution:addValue( "Bronowice", {{2020,04,22},{11,11,11}}, "PM2.5", 40.4)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( "Bronowice", {{2020,04,23},{11,11,11}}, "Temp", 15)),

  ?assertEqual({reply,'station added'}, pollution:addStation( "Azory", {50.2345, 18.3445}, test)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( {50.2345, 18.3445}, {{2020,04,21},{11,11,11}}, "PM10", 50.7)),
  ?assertEqual({reply,'value added'}, pollution:addValue("Azory", {{2020,04,22},{11,11,11}}, "PM2.5", 72.4)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( "Azory", {{2020,04,23},{11,11,11}}, "Temp", 16.1)),
  ?assertEqual({reply,'value added'}, pollution:addValue( {31.23,45.67}, {{2020,04,21},{16,11,11}}, "PM10", 10.1)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( "Bronowice", {{2020,04,22},{19,51,11}}, "PM2.5", 34.45)),
  ?assertEqual({reply,'value added'}, pollution:addValue( "Bronowice", calendar:local_time(), "Temp", 15.1)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( {50.2345, 18.3445}, {{2020,04,21},{18,11,11}}, "PM10", 50.0)),
  ?assertEqual({reply,'value added'}, pollution:addValue( "Azory", {{2020,04,22},{19,18,11}}, "PM2.5", 43.3)),
  ?assertEqual({reply,'value added'} ,pollution:addValue( "Azory", calendar:local_time(), "Temp", 15.8)),

  %%getOneValue
  ?assertEqual({reply,50.7},pollution:getOneValue({50.2345, 18.3445}, {{2020,04,21},{11,11,11}}, "PM10")), %% correct
  ?assertEqual({reply,23.4},pollution:getOneValue("Bronowice", {{2020,04,21},{11,11,11}}, "PM10")),
  ?assertEqual({reply,15},pollution:getOneValue("Bronowice", {{2020,04,23},{11,11,11}}, "Temp")),

  ?assertEqual({error,no_such_value},pollution:getOneValue({50.2345, 18.3445}, {{2020,04,21},{11,11,51}}, "PM10" )), %%wrong date
  ?assertEqual({error,wrong_station_name},pollution:getOneValue("Kielce", {{2020,04,21},{11,11,51}}, "PM10" )), %% wrong station name
    %%getStationMean
  ?assertEqual({reply,16.75}, pollution:getStationMean("Bronowice","PM10")),
  ?assertEqual({reply,57.85},pollution:getStationMean({50.2345, 18.3445},"PM2.5")),
  ?assertEqual({reply,0.0},pollution:getStationMean("Zabierzow","PM2.5")), %% wrong station name
  ?assertEqual({reply,0.0},pollution:getStationMean("Bronowice","PM222222")), %% wrong type
    %%getDailyMean
  ?assertEqual({reply,33.55}, pollution:getDailyMean({2020,04,21},"PM10")),
  ?assertEqual({reply,47.6375},pollution:getDailyMean({2020,04,22},"PM2.5")),
  ?assertEqual({reply,0.0},pollution:getDailyMean("Zabierzow","PM2.5")), %% wrong station name
  ?assertEqual({reply,0.0},pollution:getDailyMean("Bronowice","PM222222")), %% wrong type
    %%getWorstDay
  ?assertEqual({reply,{{{2020,04,21},{11,11,11}},23.4}},pollution:getWorstDay("Bronowice","PM10")),
  ?assertEqual({reply,{{{2020,04,21},{11,11,11}},50.7}},pollution:getWorstDay("Azory","PM10")),
  ?assertEqual({error,wrong_station_name},pollution:getWorstDay("Zabierzow","PM2.5")),
    %%getWorstHourlyStation
  ?assertEqual({reply,{{50.2345,18.3445},50.7}},pollution:getWorstHourlyStation({2020,04,21},11,"PM10")),
  ?assertEqual({reply,{0,0}},pollution:getWorstHourlyStation({2020,04,21},11,"PM2.5564")),
  ?assertEqual({reply,{0,0}},pollution:getWorstHourlyStation({2020,04,21},11,"PM2.5")), %% no measures such type that day

  ?assertEqual({reply, 'station removed'},pollution:removeStation("Bronowice")),
  ?assertEqual({reply, 'station removed'},pollution:removeStation("Azory")),
  pollution:stopMonitor().