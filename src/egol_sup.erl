-module(egol_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).


-define(SERVER, ?MODULE).

start_link() ->
  egol_time:init(),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,

  %% Time = {egol_time, {egol_time, init, []},
  %%         Restart, Shutdown, worker, [egol_time]}, 
  CellSup= {egol_cell_sup, {egol_cell_sup, start_link, []},
            Restart, Shutdown, supervisor, [egol_cell_sup]},
  MgrSup = {egol_cell_mgr, {egol_cell_mgr, start_link, []},
            Restart, Shutdown, worker, [egol_cell_mgr]},


  {ok, {SupFlags, [MgrSup, CellSup]}}.
