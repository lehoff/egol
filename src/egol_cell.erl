-module(egol_cell).

-compile([{parse_transform, lager_transform}]).

-export([start/2,
         init/1]).
-export([set/2,
         get/2,
         get_sync/2,
         history_sync/1,
         time_sync/1,
         run/1,
         pause/1,
         step/1]).
         

-record(state,
        { xy,
          dim,
          content=0 :: 0..1,
          time=0,
          future= [] :: [{pid(), pos_integer()}],
          history=[],
          neighbours}).

start({X,Y}=XY, {DimX, DimY}=Dim) when X < DimX;
                                       0 =< X;
                                       Y < DimY;
                                       0 =< Y ->
  spawn(?MODULE, init, [#state{xy=XY, dim=Dim, content=0,
                               neighbours=neighbours(XY, Dim)}]).


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

pause(To) -> cmd(To, pause).

step(To) -> cmd(To, step).



cmd(To, Cmd) when is_pid(To) ->
  To ! Cmd;
cmd(To, Cmd) when is_tuple(To) ->
  cmd( gproc:where(cell_name(To)), Cmd );
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
  idle(State).


idle(State) ->
  receive 
    {set, NewContent} ->
      idle(State#state{content=NewContent});
    {From, {get, Time}} ->
      case content_at(Time, State) of
        future ->
          idle(State#state{future=[{From, Time} | State#state.future]});
        C ->
          From ! {cell_content, C},              
          idle(State)
      end;
    {From, history} ->
      #state{history=History} = State,
      From ! History,
      idle(State);
    {From, time} ->
      From ! State#state.time,
      idle(State);
    run ->
      running(State);
    step ->
      NewState=run_step(State),
      idle(NewState);
    pause ->
      idle(State)    
  end.

running(#state{time=T, neighbours=Neighbours}=State) ->
  query_neighbours(T, Neighbours),
  NewState = collecting(State, 0, neighbours_at(T, Neighbours)),
  receive
    pause ->
      idle(NewState);
    step ->
      NextState = collecting(NewState, 0, Neighbours),
      idle(NextState)
  after
    0 ->
      running(NewState)
  end.

run_step(#state{time=T, neighbours=Neighbours}=State) ->
  query_neighbours(T, Neighbours),
  collecting(State, 0, neighbours_at(T, Neighbours)).

collecting(#state{xy=XY, content=Content, time=T, history=History, future=Future}=State, NeighbourCount, []) ->
  NextContent = next_content(Content, NeighbourCount),
  NewFuture = process_future(XY, T+1, NextContent, Future),
  lager:info("Cell ~p changing to ~p for time ~p", [XY, NextContent, T+1]),
  State#state{content=NextContent,
              time=T+1,
              history=[{T, Content}|History],
              future=NewFuture};
collecting(#state{}=State, NeighbourCount, WaitingOn) ->
  receive
    {From, {get, Time}} ->
      case content_at(Time, State) of
        future ->
          collecting(State#state{future=[{From, Time}|State#state.future]},
                     NeighbourCount, WaitingOn);
        C ->
          From ! {cell_content, C},
          collecting(State, NeighbourCount, WaitingOn)
      end;
    {cell_content, {{{_,_},_}=XYatT, NeighbourContent}} ->
      case lists:member(XYatT, WaitingOn) of
        true ->
          collecting(State, NeighbourCount + NeighbourContent, lists:delete(XYatT, WaitingOn));
        false %% ignore messages we are not waiting for
              ->
          collecting(State, NeighbourCount, WaitingOn)
      end;
    {set, NewContent} %% fun stuff can happen if you change the state while running...
                      ->
      collecting(State#state{content=NewContent}, NeighbourCount, WaitingOn)
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
