%%%-------------------------------------------------------------------
%% @doc pollution_with_mnesia public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_with_mnesia_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_with_mnesia_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
