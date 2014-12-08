-module(egol).
 
-behaviour(gen_server).



-export([start/3,
         step/0,
         run/0,
         run/1,
         run_until/1,
         max_time/0,
         mode/0,
         pause/0,
         kill/1,
         print/1,
         print_last/0,
         print_lag/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-export([test/1]).



-record(state,
        {size_x,
         size_y,
         mode
        }).

-define(SERVER, ?MODULE).

start(N, M, InitialCells) ->
  gen_server:start({local, ?SERVER}, ?MODULE, [N, M, InitialCells], []).

init([N, M, InitialCells]) ->
  AllCells = all_cells(N, M),
  Cells =  [{start_cell(XY, {N,M}, lists:member(XY, InitialCells)), XY}
            || XY <- AllCells ],
  egol_cell_mgr:start(Cells),
  {ok, #state{size_x=N,
              size_y=M,
              mode=step}}.

start_cell(XY, Dim, true) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, 1),
  Pid;
start_cell(XY, Dim, false) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, 0),
  Pid.

step() ->
  gen_server:cast(?SERVER, step).

run() ->
  gen_server:cast(?SERVER, run).

run(Time) ->
  run(),
  timer:sleep(Time),
  pause().

run_until(EndTime) ->
  gen_server:cast(?SERVER,{run_until, EndTime}).

max_time() ->
  gen_server:call(?SERVER,  max_time).

mode() ->
  gen_server:call(?SERVER, mode).
  
pause() ->
  gen_server:cast(?SERVER, pause).

kill(XY) ->
  egol_cell:kill(XY).

print(T) ->
  gen_server:cast(?SERVER, {print, T}).
        
print_last() ->
  gen_server:cast(?SERVER, print_last).

print_lag() ->
  gen_server:cast(?SERVER, print_lag).


handle_cast(step, State) ->
  step_cells(all_cells(State)),
  {noreply, State#state{mode=step}};
handle_cast(run, State) ->
  run_cells(all_cells(State)),
  {noreply, State#state{mode=run}};
handle_cast({run_until, EndTime}, State) ->
  run_cells_until(all_cells(State), EndTime),
  {noreply, State#state{mode={run_until,EndTime}}};
handle_cast(pause, State) ->
  pause_cells(all_cells(State)),
  {noreply, State#state{mode=step}};
handle_cast({print,T}, State) ->
  print(State#state.size_x, State#state.size_y, T),
  {noreply, State};
handle_cast(print_last, State) ->
  MinTime = minimum_time(State),
  io:format("Time is ~p.~n", [MinTime]),
  print(State#state.size_x, State#state.size_y, MinTime),
  {noreply, State};
handle_cast(print_lag, State) ->
  print_lag(State),
  {noreply, State}.


handle_call(mode, _From, State) ->
  {reply, State#state.mode, State};
handle_call(max_time, _From, State) ->
  {reply, maximum_time(State), State}.


handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




maximum_time(State) ->
  AllCellPids = [ egol_cell:where(Cell) 
                  || Cell <- all_cells(State) ],
  LiveCells = lists:filter( fun erlang:is_pid/1, AllCellPids),
  AllTimes = [ egol_cell:time(Cell) 
               || Cell <- LiveCells ],
  lists:max(AllTimes).
                                

minimum_time(State) ->
  Times = all_times(State),
  lists:min(Times).

all_times(State) ->
  Tagged = all_times_tagged(State),
  [ T || {_, T} <- Tagged ].

all_times_tagged(State) ->
  [ {XY, egol_cell:time(XY)}
    || XY <- all_cells(State) ].


fill_cells(Cells) ->
  [ egol_cell:set(Cell, 1)
    || Cell <- Cells].


step_cells(Cells) -> lists:foreach(fun egol_cell:step/1, Cells).

run_cells(Cells) -> lists:foreach(fun egol_cell:run/1, Cells).

run_cells_until(Cells, EndTime) ->
  lists:foreach(fun (Cell) ->
                    egol_cell:run_until(Cell, EndTime)
                end,
                Cells).

pause_cells(Cells) -> lists:foreach(fun egol_cell:pause/1, Cells).



print(N, M, Time) ->
  Cells = all_cells(N, M),
  CellsAtT = cells_at(Time, Cells),
  Board = query_cells(CellsAtT),
  lists:foreach( fun (K) ->
                   print_row(K, N, Board)
               end,
               lists:seq(M-1, 0, -1) ).

print_row(K, N, Board) ->
  Values = [ cell_content({N1,K}, Board)
             || N1 <- lists:seq(0, N-1) ],
  [format_cell_value(Value)
   || Value <- Values],
  io:format("~n").

print_lag(#state{size_x=N, size_y=M}=State) ->
  TaggedTimes = all_times_tagged(State),
  Times = all_times(State),
  MaxTime = lists:max(Times),
  Lag = [ {XY, MaxTime - T}
          || {XY, T} <- TaggedTimes ],
  FormattedLag = [ {XY, format_lag(L)}
                   || {XY, L} <- Lag ],
  print_values(N, M, FormattedLag).

print_values(N, M, Board) ->
  lists:foreach( fun (K) ->
                     print_formatted_row(K, N, Board)
                 end,
                 lists:seq(M-1, 0, -1) ).
                 
print_formatted_row(K, N, Board) ->
  Values = [ cell_content({N1,K}, Board)
             || N1 <- lists:seq(0, N-1) ],
  [ io:format("~s", [Value])
    || Value <- Values],
  io:format("~n").
  

format_lag(N) when N < 10 ->
  io_lib:format("~p", [N]);
format_lag(_) ->
  "+".
  

format_cell_value(0) -> io:format(".");
format_cell_value(1) -> io:format("*").
                 

all_cells(N, M) ->
  [ {X, Y}
    || X <- lists:seq(0, N-1),
       Y <- lists:seq(0, M-1) ].

all_cells(#state{size_x=N, size_y=M}) ->
  all_cells(N, M).

cells_at(Time, Cells) ->
  [ {C, Time} || C <- Cells ].

query_cells(CellsAtT) ->
  [ {XY, egol_cell:get(XY, T)}
   || {XY, T} <- CellsAtT].

cell_content(XY, Board) ->
  proplists:get_value(XY, Board).

test(1) ->
  start(8, 8, []);
test(2) ->
  [{0,0}, {1,0}, {2,0}, {2,1},{1,2}];
test(3) ->
  egol_cell_sup:start_link(),
  start(8,8,test(2));
test(4) ->
  test(3),
  timer:sleep(50),
  run(200),
  kill({0,0}),
  run(200);
test(5) ->
  test(3),
  timer:sleep(50),
  run(),
  timer:sleep(200),
  kill({1,1}),
  timer:sleep(200),
  pause(),
  [ kill(C) 
    || C <- [{0,2}, {1,2}, {2,2},
             {0,1}, {1,1}, {2,1},
             {0,0}, {1,0}, {2,0}]];
test(6) ->
  InitialCells = [{2,3}, {2,5},
                  {3,2}, {3,3}, {3,4},
                  {4,1}, {4,5}],
  egol_cell_sup:start_link(),
  start(7,6, InitialCells).
       
  

  
  
