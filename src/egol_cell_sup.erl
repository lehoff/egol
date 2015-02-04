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
  {ok, _Pid} = Res = supervisor:start_child(?SERVER, child_spec(XY, Dim, InitialContent)),
%%  egol_cell_mgr:reg(XY, Pid),
  Res.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 1,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  %% Restart = transient,
  %% Shutdown = 10000,
  %% Type = worker,

  %% AChild = {egol_cell, {egol_cell, start_link, []},
  %%           Restart, Shutdown, Type, [egol_cell]},

  {ok, {SupFlags, []}}.


child_spec(XY, Dim, InitialContent) ->
  {{egol_cell, XY}, {egol_cell, start_link, [XY, Dim, InitialContent]},
   transient, infinity, worker, [egol_cell]}.


