-module(egol_cell).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-export([start/3,
         start_link/3,
         kill/1,
         where/1]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).


-export([set/2,
         get/2,
         history/1,
         time/1,
         run/1,
         run_until/2,
         pause/1,
         step/1]).

-type cell_content() :: 0 | 1.
-type cell_name() :: {integer(), integer()}.
-type time() :: integer().

         
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
  gen_server:start(?MODULE,  
                   #state{xy=XY, dim=Dim, content=InitialContent,
                          neighbours=egol_util:neighbours(XY, Dim)},
                   []).

start_link({X,Y}=XY, {DimX, DimY}=Dim, InitialContent) 
  when X < DimX;
       0 =< X;
       Y < DimY;
       0 =< Y ->
  gen_server:start_link(?MODULE, 
                        #state{xy=XY, dim=Dim, content=InitialContent,
                                neighbours=egol_util:neighbours(XY, Dim)},
                        []).


where(XY) ->
  egol_cell_mgr:lookup(XY).

kill(XY) ->
  case where(XY) of
    undefined ->
      ok;
    Pid when is_pid(Pid) ->
      exit(Pid, kill)
  end.

set(Cell, Content) ->
  cast(Cell, {set, Content}).


-spec get(pid()|cell_name(), time()) -> cell_content().
get(Cell, Time) ->
  call(Cell, {get, Time}).


history(Cell) ->
  call(Cell, history).

time(Cell) ->
  call(Cell, time).


run(Cell) ->
  cast(Cell, run).


run_until(Cell, EndTime) ->
  cast(Cell, {run_until, EndTime}).

pause(Cell) -> 
  cast(Cell, pause).

step(Cell) ->
  cast(Cell, step).



call(Cell, Cmd) ->
  gen_server:call(cell_pid(Cell), Cmd).

cast(Cell, Cmd) ->
  gen_server:cast(cell_pid(Cell), Cmd).

cell_pid({_,_}=XY) ->
  egol_cell_mgr:lookup(XY);
cell_pid(Pid) when is_pid(Pid) ->
  Pid.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(#state{xy=XY}=State) ->
  egol_time:set(XY, 0),
  {ok, State}.


handle_cast({set, NewContent}, State) ->
  {noreply, State#state{content=NewContent}};
handle_cast({From, {get, Time}}, State) ->
  case content_at(Time, State) of
    future ->
      {noreply, State#state{future=[{From, Time} | State#state.future]}};
    C ->
      From ! {cell_content, C},              
      {noreply, State}
  end;
handle_cast(run, State) ->
  case is_collector_running(State) of
    true ->
      {noreply, State#state{mode=run}};
    false ->
      NextState = start_collector(State),
      {noreply,  NextState#state{mode=run}}
  end;
handle_cast(step, State) ->
  case is_collector_running(State) of
    true ->
      {noreply, State#state{mode=step}};
    false ->
      NewState = start_collector(State),
      {noreply, NewState#state{mode=step}}
  end;
handle_cast({run_until, EndTime}, 
            #state{time=T}=State) ->
  case is_collector_running(State) of
    true ->
      {noreply, State#state{mode={run_until, EndTime}}};
    false ->
      case T < EndTime of
        true ->
          NewState = start_collector(State),
          {noreply, NewState#state{mode={run_until, EndTime}}};
        false ->
          {noreply, State}
      end
  end;
handle_cast(pause, State) ->
  {noreply, State#state{mode=step}}.

handle_call(history, _From, State) ->
  {reply, State#state.history, State};
handle_call(time, _From, State) ->
  {reply, State#state.time, State};
handle_call({get, Time}, _From, State) ->
  case content_at(Time, State) of
    future ->
      {reply, future, State};
    {_, C} ->
      {reply, C, State}
  end.

  

handle_info({Collector, {next_content, NextContent}}, 
            #state{collector=Collector,
                   future=Future, xy=XY, time=T,
                   content=Content, history=History}=State) ->
  NewFuture = process_future(XY, T+1, NextContent, Future),
  lager:debug("Cell ~p changing to ~p for time ~p", [XY, NextContent, T+1]),
  egol_time:set(XY, T+1),
  NextState = State#state{content=NextContent,
                          time=T+1,
                          history=[{T, Content}|History],
                          future=NewFuture}, 
  case State#state.mode of
    step ->
      {noreply, NextState};
    run ->
      {noreply, start_collector(NextState)};
    {run_until, EndTime} ->
      case NextState#state.time < EndTime of
        true ->
          {noreply, start_collector(NextState)};
        false ->
          {noreply, NextState#state{mode=step}}
      end
  end;
handle_info({query_content, Time, From}, State) ->
  case content_at(Time, State) of
    future ->
      {noreply, State#state{future=[{From, Time} | State#state.future]}};
    C ->
      egol_protocol:query_response(From, {cell_content, C}),              
      {noreply, State}
  end.


terminate(_Reason, State) ->
  egol_time:clear(State#state.xy),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

is_collector_running(#state{collector=Collector}) ->
    is_pid(Collector) andalso is_process_alive(Collector).


start_collector(#state{time=T, neighbours=Neighbours, 
                       content=Content, xy=XY}=State) ->
  lager:debug("start_collector: neighbours=~p~n", [Neighbours]),
  Cell = self(),
  Collector = spawn_link( fun () ->
                         collector_init(XY, T, Neighbours, Cell, Content)
                     end ),
  State#state{collector=Collector}.

collector_init(XY, Time, Neighbours, Cell, Content) ->
  query_neighbours(Time, Neighbours),
  collector_loop(egol_util:neighbours_at(Time, Neighbours), 0, Cell, Content).

collector_loop([], NeighbourCount, Cell, Content) ->
  Cell ! {self(), {next_content, egol_util:next_content(Content, NeighbourCount)}};
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
      end
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
                     egol_protocol:query_content(N, T)
                 end,
                 Neighbours).
                                                                    
  

content_at(Time, #state{xy=XY, time=Time, content=Content}) ->
  {{XY,Time}, Content};
content_at(Time, #state{time=T}) when Time > T ->
  future;
content_at(Time, #state{xy=XY, history=History}) when is_integer(Time), Time >= 0->
  {_, Content} = lists:keyfind(Time, 1, History),
  {{XY, Time}, Content}.

