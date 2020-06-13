%%%-------------------------------------------------------------------
%%% @author Natalia
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. cze 2020 18:10
%%%-------------------------------------------------------------------
-module(mnesia_database_tests).
-author("Natalia").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

%%
%%start_stop_test() ->
%%  ?assertEqual(ok, mnesia_database:startDB()),
%%  ?assertEqual(stopped, mnesia_database:stopDB()).

addStationDB_test() ->
%%  mnesia_database:initDB(),
  mnesia_database:startDB(),
  ?assertEqual(ok, mnesia_database:addStationDB("Stacja1", {123.21,213.32},"Airly")),
  ?assertError(same_station_attributes, mnesia_database:addStationDB("Stacja1", {111.11,222.22},"Airly")),
  ?assertError(same_station_attributes, mnesia_database:addStationDB("Stacja2", {343.21,213.32},"Airly")),
  ?assertEqual(ok, mnesia_database:addStationDB("Stacja", {343.21,213.32},"WIOS")),
  mnesia_database:stopDB().