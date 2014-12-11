-module(egol_cell_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_cell/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_cell(XY, Dim, InitialContent) ->
  {ok, Pid} = Res = supervisor:start_child(?SERVER, [XY, Dim, InitialContent]),
  egol_cell_mgr:reg(XY, Pid),
  Res.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {egol_cell, {egol_cell, start_link, []},
            Restart, Shutdown, Type, [egol_cell]},

  {ok, {SupFlags, [AChild]}}.



