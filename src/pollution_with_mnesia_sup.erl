%%%-------------------------------------------------------------------
%% @doc pollution_with_mnesia top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_with_mnesia_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
%%  mnesia:wait_for_tables([measurements, stations],5000),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  SupFlags = #{strategy => one_for_one,
    intensity => 1,
    period => 1},

  ChildSpecs = #{id => 'poll_srv',
    start => {pollution_gen_server,start,[]},
    restart => transient,
    shutdown => 900000,
    type => worker,
    modules => [pollution,pollution_gen_server, mnesia_database]},
  {ok, {SupFlags, [ChildSpecs]}}.

%% internal functions
