%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Natalia Brzozowska").

%% API
-export([start/0, stop/0,crush/0, initDB/1,addStation/2, addStation/3, addValue/4, removeValue/3,getAllStations/0, getDailyMean/2, getOneValue/3, getStationMean/2, getWorstDay/2, getWorstHourlyStation/3, handle_cast/2, removeStation/1, getStationAttributes/1, getStationAllMeasurements/1, getAllHourMeasurements/2]).
-export([init/1, handle_call/3, check_error/2,terminate/2]).
-behaviour(gen_server).


%% START
start() ->
  gen_server:start_link({local,pollution_gen_server},pollution_gen_server,[],[]).

init([]) ->
  {ok,[]}.

%% CLIENT
crush() -> gen_server:cast(pollution_gen_server, {crush, []}).
initDB(Nodes) -> gen_server:call(pollution_gen_server, {initDB, [Nodes]}).
addStation(Name, Coordinates) -> gen_server:call(pollution_gen_server,{addStation,[Name,Coordinates, no_type]}).
addStation(Name, Coordinates, Type) -> gen_server:call(pollution_gen_server,{addStation,[Name,Coordinates, Type]}).
addValue(Id, Date, Type, Value)-> gen_server:call(pollution_gen_server,{addValue,[Id,Date, Type, Value]}).
removeValue(Id, Date, Type)-> gen_server:call(pollution_gen_server,{removeValue,[Id, Date, Type]}).
getOneValue(Id, Date, Type)-> gen_server:call(pollution_gen_server,{getOneValue,[Id, Date, Type]}).
getStationMean(Id, Type)-> gen_server:call(pollution_gen_server,{getStationMean,[Id, Type]}).
getDailyMean(Day,Type)-> gen_server:call(pollution_gen_server,{getDailyMean, [Day, Type]}).
getWorstDay(Id, Type)-> gen_server:call(pollution_gen_server,{getWorstDay,[Id, Type]}).
getWorstHourlyStation(Day, Hour, Type)-> gen_server:call(pollution_gen_server,{getWorstHourlyStation,[Day, Hour, Type]}).
stop() -> gen_server:call(pollution_gen_server,terminate).
removeStation(Id) ->gen_server:call(pollution_gen_server, {removeStation, [Id]}).
getStationAttributes(Id) ->gen_server:call(pollution_gen_server, {getStationAttributes, [Id]}).
getStationAllMeasurements(Id) ->gen_server:call(pollution_gen_server, {getStationAllMeasurements, [Id]}).
getAllHourMeasurements(Day, Hour) ->gen_server:call(pollution_gen_server, {getAllHourMeasurements, [Day, Hour]}).
getAllStations() -> gen_server:call(pollution_gen_server, {getAllStations, []}).
%% SERVER

check_error(error,Msg) ->io:format("received error - ~s ~n",[Msg]);
check_error(reply,_)->ok.


handle_call({initDB, [Nodes]},_From,_) ->
  {Atom, _} = pollution:startMonitor(Nodes),
  {reply,Atom,[]};
handle_call({addStation,[Name,Coordinates, Type]},_From,_) ->
  {Atom, Msg} = pollution:addStation(Name,Coordinates, Type),
  check_error(Atom,Msg),
  {reply,Msg,[]};
handle_call({addValue,[Id,Date, Type, Value]}, _From,_) ->
  {Atom, Msg}=pollution:addValue(Id,Date, Type,Value),
  check_error(Atom,Msg),
  {reply,Msg,[]};
handle_call({removeValue,[Id, Date, Type]},_From,_) ->
  {Atom, Msg}=pollution:removeValue(Id,Date, Type),
  check_error(Atom,Msg),
  {reply,Msg,[]};
handle_call({getOneValue,[Id, Date, Type]}, _From,_) ->
  {Atom,Val}=pollution:getOneValue(Id,Date,Type),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getStationMean,[Id, Type]}, _From, _) ->
  {Atom,Val}=pollution:getStationMean(Id,Type),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getDailyMean, [Day, Type]}, _From, _) ->
  {Atom,Val}=pollution:getDailyMean(Day,Type),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getWorstDay,[Id, Type]}, _From, _) ->
  {Atom,Val}=pollution:getWorstDay(Id,Type),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getWorstHourlyStation,[Day, Hour, Type]}, _From, _) ->
  {Atom,Val}=pollution:getWorstHourlyStation(Day, Hour,Type),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({removeStation,[Id]},_From,_) ->
  {Atom, Msg}=pollution:removeStation(Id),
  check_error(Atom,Msg),
  {reply,Msg,[]};
handle_call({getStationAttributes,[Id]}, _From,_) ->
  {Atom,Val}=pollution:getStationAttributes(Id),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getStationAllMeasurements,[Id]}, _From, _) ->
  {Atom,Val}=pollution:getStationAllMeasurements(Id),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getAllHourMeasurements, [Day, Hour]}, _From, _) ->
  {Atom,Val}=pollution:getAllHourMeasurements(Day,Hour),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call({getAllStations, []}, _From, _) ->
  {Atom,Val}=pollution:getAllStations(),
  check_error(Atom,Val),
  {reply,Val,[]};
handle_call(terminate, _From, _) ->
  {stop,normal,stopped,[]}.

terminate(normal,_) -> pollution:stopMonitor([node()]), io:format("Closing monitor. Reason is normal ~n"),ok;
terminate(other,_) -> pollution:stopMonitor([node()]), io:format("Closing monitor. Reason is other ~n"),ok;
terminate(_,_) -> pollution:stopMonitor([node()]), io:format("Closing monitor. Why? I dont know :(( ~n"),ok.



handle_cast(aaa, _) ->
  io:format("Handle cast. ~n"),
  {noreply,[]}.