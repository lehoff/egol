-module(egol_cell_mgr).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).


-export([start/0]).

-export([reg/2,
         lookup/1]).


%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).



-type cell_coordinates() :: {integer(), integer()}.


-record(state,
        {cells =[] :: [{pid(), reference(), cell_coordinates()}]
        }).

start() ->
  gen_server:start({local,?MODULE}, ?MODULE, [], []).


reg(XY, Pid) when is_pid(Pid) ->
  gen_server:cast(?MODULE, {reg, XY, Pid}).

lookup(XY) ->
  gen_server:call(?MODULE, {lookup, XY}).

init([]) ->
  {ok, #state{}}.

handle_call({lookup, XY}, _From, #state{cells=Cells}=State) ->
  Reply = 
    case lists:keyfind(XY, 3, Cells) of
        false ->
        undefined;  %% @todo queue until registered
      {Pid, _Ref, XY} ->
          Pid
    end,
  {reply, Reply, State}.

handle_cast({reg, XY, Pid}, #state{cells=Cells}=State) ->
  NewRef = erlang:monitor(process, Pid),
  NextState = State#state{cells=[{Pid, NewRef, XY}|Cells]},
  spawn ( fun () -> pacer(Pid) end ),
  {noreply, NextState}.

handle_info({'DOWN', Ref, process, _Pid, _Info}, 
            #state{cells=Cells}=State) ->
  Rest = lists:keydelete(Ref, 2, Cells),
  {noreply, State#state{cells=Rest}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%% loop(#state{cells=Cells}=State) ->
%%   receive 
%%     {'DOWN', Ref, process, _Pid, _Info} ->
%%        Rest = lists:keydelete(Ref, 2, Cells),
%%       loop(State#state{cells=Rest});
%%       %% NewPid = await_restart(XY),
%%       %% NewRef = erlang:monitor(process, NewPid),
%%       %% NextState = State#state{cells=[{NewPid, NewRef, XY}|Rest]},
%%       %% spawn ( fun () -> pacer(NewPid) end ),
%%       %% loop(NextState)
%%     {reg, XY, Pid} ->
%%       NewRef = erlang:monitor(process, Pid),
%%       NextState = State#state{cells=[{Pid, NewRef, XY}|Cells]},
%%       spawn ( fun () -> pacer(Pid) end ),
%%       loop(NextState);
%%     {lookup, XY, From} ->
%%       case lists:keyfind(XY, 3, Cells) of
%%         false ->
%%           From ! undefined;  %% @todo queue until registered
%%         {Pid, _Ref, XY} ->
%%           From ! Pid
%%       end,
%%       loop(State)
%%   end.

pacer(Pid) ->
  case egol:mode() of
    step ->
      EndTime = egol:max_time(),
      egol_cell:run_until(Pid, EndTime);
    {run_until, EndTime} ->
      egol_cell:run_until(Pid, EndTime);
    run ->
      egol_cell:run(Pid)
  end.

%% await_restart(XY) ->
%%   timer:sleep(10),
%%   case egol_cell:where(XY) of
%%     undefined ->
%%       await_restart(XY);
%%     Pid ->
%%       Pid
%%   end.
      
