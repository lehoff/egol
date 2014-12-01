-module(egol_cell).

-compile([{parse_transform, lager_transform}]).

-export([start/3,
         start_link/3,
         init/1,
         kill/1,
         where/1]).
-export([set/2,
         get/2,
         get_sync/2,
         history_sync/1,
         time_sync/1,
         run/1,
         run_until/2,
         pause/1,
         step/1]).
         
-type mode() :: 'run' | 'step'.

-record(state,
        { xy,
          dim,
          content=0 :: 0..1,
          time=0,
          collector :: pid() | 'undefined',
          pacer :: pid() | 'undefined',
          mode=step :: mode(),
          future= [] :: [{pid(), pos_integer()}],
          history=[],
          neighbours}).

start({X,Y}=XY, {DimX, DimY}=Dim, InitialContent) 
  when X < DimX;
       0 =< X;
       Y < DimY;
       0 =< Y ->
  spawn(?MODULE, init, [#state{xy=XY, dim=Dim, content=InitialContent,
                               neighbours=neighbours(XY, Dim)}]).

start_link({X,Y}=XY, {DimX, DimY}=Dim, InitialContent) 
  when X < DimX;
       0 =< X;
       Y < DimY;
       0 =< Y ->
  Pid = spawn_link(?MODULE, init, [#state{xy=XY, dim=Dim, content=InitialContent,
                                          neighbours=neighbours(XY, Dim)}]),
  {ok, Pid}.


where(XY) ->
  gproc:where(cell_name(XY)).

kill(XY) ->
  case where(XY) of
    undefined ->
      ok;
    Pid when is_pid(Pid) ->
      exit(Pid, kill)
  end.

set(To, State) ->
  cmd(To, {set, State}).

get(To, Time) ->
  cmd(To, {self(), {get, Time}}).

get_sync(To, Time) ->
  {cell_content, C} = cmd_sync(To, {get, Time}),
  C.

history_sync(To) ->
  cmd_sync(To, history).

time_sync(To) ->
  cmd_sync(To, time).

run(To) -> cmd(To, run).

run_until(To, EndTime) ->
  cmd(To, {run_until, EndTime}).

pause(To) -> cmd(To, pause).

step(To) -> cmd(To, step).



cmd(To, Cmd) when is_pid(To) ->
  To ! Cmd;
cmd(To, Cmd) when is_tuple(To) ->
  cmd( where(To), Cmd );
cmd(To, Cmd) ->
  lager:error("Incorrect To:~p with Cmd:~p", [To, Cmd]).

cmd_sync(To, Cmd) ->
  cmd(To, {self(), Cmd}),
  receive
    Res ->
      Res
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% states
init(#state{xy=XY}=State) ->
  reg(XY),
  loop(State).


loop(#state{xy=XY, time=T, content=Content, collector=Collector,
            future=Future, history=History}=State) ->
  receive 
    {set, NewContent} ->
      loop(State#state{content=NewContent});
    {From, {get, Time}} ->
      case content_at(Time, State) of
        future ->
          loop(State#state{future=[{From, Time} | State#state.future]});
        C ->
          From ! {cell_content, C},              
          loop(State)
      end;
    {From, history} ->
      #state{history=History} = State,
      From ! History,
      loop(State);
    {From, time} ->
      From ! State#state.time,
      loop(State);
    run ->
      case is_collector_running(State) of
        true ->
          loop(State#state{mode=run});
        false ->
          NextState = start_collector(State),
          loop(NextState#state{mode=run})
      end;
    step ->
      case is_collector_running(State) of
        true ->
          loop(State#state{mode=step});
        false ->
          NewState = start_collector(State),
          loop(NewState#state{mode=step})
      end;
    {run_until, EndTime} ->
      case is_collector_running(State) of
        true ->
          loop(State#state{mode={run_until, EndTime}});
        false ->
          case T < EndTime of
            true ->
              NewState = start_collector(State),
              loop(NewState#state{mode={run_until, EndTime}});
            false ->
              loop(State)
          end
      end;
    pause ->
      loop(State#state{mode=step});
    {Collector, {next_content, NextContent}} ->
      NewFuture = process_future(XY, T+1, NextContent, Future),
      lager:info("Cell ~p changing to ~p for time ~p", [XY, NextContent, T+1]),
      NextState = State#state{content=NextContent,
                              time=T+1,
                              history=[{T, Content}|History],
                              future=NewFuture}, 
      case State#state.mode of
        step ->
          loop(NextState);
        run ->
          loop(start_collector(NextState));
        {run_until, EndTime} ->
          case NextState#state.time < EndTime of
            true ->
              loop(start_collector(NextState));
            false ->
              loop(NextState#state{mode=step})
          end
      end
  end.

is_collector_running(#state{collector=Collector}) ->
    is_pid(Collector) andalso is_process_alive(Collector).

is_pacer_running(#state{pacer=Pacer}) ->
    is_pid(Pacer) andalso is_process_alive(Pacer).

start_collector(#state{time=T, neighbours=Neighbours, content=Content}=State) ->
  Cell = self(),
  Collector = spawn( fun () ->
                         collector_init(T, Neighbours, Cell, Content)
                     end ),
  State#state{collector=Collector}.

collector_init(Time, Neighbours, Cell, Content) ->
  query_neighbours(Time, Neighbours),
  collector_loop(neighbours_at(Time, Neighbours), 0, Cell, Content).

collector_loop([], NeighbourCount, Cell, Content) ->
  Cell ! {self(), {next_content, next_content(Content, NeighbourCount)}};
collector_loop(WaitingOn, NeighbourCount, Cell, Content) ->
  receive
    {cell_content, {{{_,_},_}=XYatT, NeighbourContent}} ->
      case lists:member(XYatT, WaitingOn) of
        true ->
          collector_loop(lists:delete(XYatT, WaitingOn),
                         NeighbourCount + NeighbourContent,
                         Cell, Content);
        false %% ignore messages we are not waiting for
              ->
          collector_loop(WaitingOn, NeighbourCount, Cell, Content)
      end;
    {set, NewContent} -> %% this could be fun...
      collector_loop(WaitingOn, NeighbourCount, Cell, NewContent)
  end.


process_future(XY, Time, Content, Future) ->
  {Ready, NewFuture} = lists:partition( fun({_Pid,T}) ->
                                            T == Time
                                        end,
                                        Future),
  lists:foreach( fun({Pid,_}) ->
                     Pid ! {cell_content, {{XY,Time}, Content}}
                 end,
                 Ready),
  NewFuture.

query_neighbours(T, Neighbours) ->
  lists:foreach( fun(N) ->
                   get(N, T)
               end,
               Neighbours).

next_content(1, 2) -> 1;
next_content(_, 3) -> 1;
next_content(_, _) -> 0.
                                                                    
  

content_at(Time, #state{xy=XY, time=Time, content=Content}) ->
  {{XY,Time}, Content};
content_at(Time, #state{time=T}) when Time > T ->
  future;
content_at(Time, #state{xy=XY, history=History}) when is_integer(Time), Time >= 0->
  {_, Content} = lists:keyfind(Time, 1, History),
  {{XY, Time}, Content}.

reg(XY) ->
  gproc:reg(cell_name(XY)).

cell_name(XY) -> {n,l,XY}.



neighbours({X,Y}, {DimX, DimY}) ->
  [ {Xa rem DimX, Ya rem DimY} ||
    Xa <- lists:seq(X-1+DimX, X+1+DimX),
    Ya <- lists:seq(Y-1+DimY, Y+1+DimY),
    {Xa,Ya} /= {X+DimX,Y+DimY}].

neighbours_at(T, Neighbours) ->
  [ {N, T} || N <- Neighbours ].
