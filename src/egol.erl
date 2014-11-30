-module(egol).
 
-export([start/3,
         init/3,
         step/0,
         run/0,
         run/1,
         run_until/1,
         pause/0,
         print/1,
         print_last/0,
         print_lag/0]).

-export([test/1]).


-record(state,
        {size_x,
         size_y}).

start(N, M, InitialCells) ->
  spawn(?MODULE, init, [N, M, InitialCells]).

init(N, M, InitialCells) ->
  AllCells = all_cells(N, M),
  [start_cell(XY, {N,M}, lists:member(XY, InitialCells))
   || XY <- AllCells ],
  register(?MODULE, self()),
  loop(#state{size_x=N,
              size_y=M}).

start_cell(XY, Dim, true) ->
  egol_cell:start(XY, Dim, 1);
start_cell(XY, Dim, false) ->
  egol_cell:start(XY, Dim, 0).

step() ->
  ?MODULE ! step.

run() ->
  ?MODULE ! run.

run(Time) ->
  run(),
  timer:sleep(Time),
  pause().

run_until(EndTime) ->
  ?MODULE ! {run_until, EndTime}.

pause() ->
  ?MODULE ! pause.

print(T) ->
  ?MODULE ! {print, T}.
        
print_last() ->
  ?MODULE ! print_last.

print_lag() ->
  ?MODULE ! print_lag.

loop(State) ->
  receive
    step ->
      step_cells(all_cells(State)),
      loop(State);
    run ->
      run_cells(all_cells(State)),
      loop(State);
    {run_until, EndTime} ->
      run_cells_until(all_cells(State), EndTime),
      loop(State);
    pause ->
      pause_cells(all_cells(State)),
      loop(State);
    {print,T} ->
      print(State#state.size_x, State#state.size_y, T),
      loop(State);
    print_last ->
      MinTime = minimum_time(State),
      io:format("Time is ~p.~n", [MinTime]),
      print(State#state.size_x, State#state.size_y, MinTime),
      loop(State);
    print_lag ->
      print_lag(State),
      loop(State)
  end.


minimum_time(State) ->
  Times = all_times(State),
  lists:min(Times).

all_times(State) ->
  Tagged = all_times_tagged(State),
  [ T || {_, T} <- Tagged ].

all_times_tagged(State) ->
  [ {XY, egol_cell:time_sync(XY)}
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
  [ begin
      {_, C} = egol_cell:get_sync(XY, T),
      {XY, C}
    end
    || {XY, T} <- CellsAtT ].

cell_content(XY, Board) ->
  proplists:get_value(XY, Board).

test(1) ->
  start(8, 8, []);
test(2) ->
  [{0,0}, {1,0}, {2,0}, {2,1},{1,2}];
test(3) ->
  start(8,8,test(2)).
  

  
  
