-module(egol).

-compile(export_all).

-record(state,
        {size_x,
         size_y}).

start(N, M, InitialCells) ->
  spawn(?MODULE, init, [N, M, InitialCells]).

init(N, M, InitialCells) ->
  AllCells = all_cells(N, M),
  [egol_cell:start(XY, {N,M})
   || XY <- AllCells ],
  timer:sleep(10),
  fill_cells(InitialCells),
  register(?MODULE, self()),
  loop(#state{size_x=N,
              size_y=M}).



step() ->
  ?MODULE ! step.

run() ->
  ?MODULE ! run.

run(Time) ->
  run(),
  timer:sleep(Time),
  pause().

pause() ->
  ?MODULE ! pause.

print(T) ->
  ?MODULE ! {print, T}.
        
print_last() ->
  ?MODULE ! print_last.

loop(State) ->
  receive
    step ->
      step_cells(all_cells(State)),
      loop(State);
    run ->
      run_cells(all_cells(State)),
      loop(State);
    pause ->
      pause_cells(all_cells(State)),
      loop(State);
    {print,T} ->
      print(State#state.size_x, State#state.size_y, T),
      loop(State);
    print_last ->
      Last = query_time(State),
      io:format("Time is ~p.~n", [Last]),
      print(State#state.size_x, State#state.size_y, Last),
      loop(State)
  end.


query_time(State) ->
  Times = lists:map( fun egol_cell:time_sync/1,
                     all_cells(State)),
  lists:min(Times).


fill_cells(Cells) ->
  [ egol_cell:set(Cell, 1)
    || Cell <- Cells].

step_all(N, M) ->
  Cells = all_cells(N, M),
  step_cells(Cells).

step_cells(Cells) -> lists:foreach(fun egol_cell:step/1, Cells).

run_all(N,M) ->
  Cells = all_cells(N, M),
  run_cells(Cells).

run_cells(Cells) -> lists:foreach(fun egol_cell:run/1, Cells).

stop_all(N,M) ->
  Cells = all_cells(N, M),
  pause_cells(Cells).

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
  Full = [{0,0}, {1,0}, {2,0}, {2,1},{1,2}];
test(3) ->
  start(8,8,test(2));
test({print,T}) ->
  print(8,8,T);
test(step) ->
  step_all(8,8);
test(run) ->
  run_all(8,8);
test(stop) ->
  stop_all(8,8);
test(4) ->
  test(1),
  timer:sleep(10),
  test(2),
%  percept_profile:start("egol-profile.dat", [procs]),
  test(run),
  timer:sleep(15000),
  test(stop);
test(5) ->
  percept:profile("egol-profile.dat", {?MODULE, test, [3]}, [procs]).
%  percept_profile:stop().
  

  
  
