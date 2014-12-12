-module(egol_cell_mgr).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).


-export([start/0]).

-export([reg/2,
         lookup/1]).

-export([count/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).



-type cell_coordinates() :: {integer(), integer()}.


-record(state,
        { monitors %%:: map {reference(), cell_coordinates()}
        }).

start() ->
  gen_server:start({local,?MODULE}, ?MODULE, [], []).


reg(XY, Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {reg, XY, Pid}).

lookup(XY) ->
  case ets:lookup(mgr_xy, XY) of
    [] ->
      undefined;
    [{XY, Pid}] ->
      Pid
  end.

count() ->
  gen_server:call(?MODULE, count).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
  ets:new(mgr_xy, [named_table, 
                   {read_concurrency, true}]),                   
  {ok, #state{monitors=gb_trees:empty()}}.

handle_call(count, _From, State) ->
  C1 = ets:size(mgr_xy),
  C2 = gb_trees:size(State#state.monitors),
  {reply, {C1,C2}, State}.

handle_cast({reg, XY, Pid}, 
            #state{monitors=Monitors}=State) ->
  lager:debug("mgr reg ~p ~p", [XY, Pid]),
  NewRef = erlang:monitor(process, Pid),
  ets:insert(mgr_xy, {XY, Pid}),
  NextState = State#state{monitors=gb_trees:enter(NewRef, XY, Monitors)},
  spawn ( fun () -> pacer(Pid) end ),
  {noreply, NextState}.

handle_info({'DOWN', Ref, process, _Pid, _Info}, 
            #state{monitors=Monitors}=State) ->
  {value, XY} = gb_trees:lookup(Ref, Monitors),
  ets:delete(mgr_xy, XY),
  {noreply, State#state{monitors=gb_trees:delete(Ref, Monitors)}}.

terminate(_Reason, _State) ->
  ets:delete(mgr_xy).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




pacer(Pid) ->
  case egol:mode() of
    not_started ->
      lager:debug("pacer with mode not_started"),
      ok;
    step ->
      EndTime = egol:max_time(),
      egol_cell:run_until(Pid, EndTime);
    {run_until, EndTime} ->
      egol_cell:run_until(Pid, EndTime);
    run ->
      egol_cell:run(Pid)
  end.

      
