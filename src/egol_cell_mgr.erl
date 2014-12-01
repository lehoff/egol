-module(egol_cell_mgr).

-export([start/1,
         init/1]).


-type cell_coordinates() :: {integer(), integer()}.


-record(state,
        {cells :: [{pid(), reference(), cell_coordinates()}]
        }).

start(Cells) ->
  spawn(?MODULE, init, [Cells]).



init(Cells) ->
  MyCells = [ begin
                Ref = erlang:monitor(process, Pid),
                {Pid, Ref, XY}
              end
              || {Pid, XY} <- Cells ], 
  loop(#state{cells=MyCells}).

loop(#state{cells=Cells}=State) ->
  receive 
    {'DOWN', Ref, process, _Pid, _Info} ->
      {value, {_,_, XY}, Rest} = lists:keytake(Ref, 2, Cells),
      NewPid = await_restart(XY),
      NewRef = erlang:monitor(process, NewPid),
      NextState = State#state{cells=[{NewPid, NewRef, XY}|Rest]},
      spawn ( fun () -> pacer(NewPid) end ),
      loop(NextState)
  end.

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

await_restart(XY) ->
  timer:sleep(10),
  case egol_cell:where(XY) of
    undefined ->
      await_restart(XY);
    Pid ->
      Pid
  end.
      
