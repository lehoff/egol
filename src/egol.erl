-module(egol).

-compile(export_all).

start(N, M, InitialCells) ->
  [ egol_cell:start({X,Y}, {N,M})
    || X <- lists:seq(0,N-1),
       Y <- lists:seq(0,M-1) ].

fill_cells(Cells) ->
  [ egol_cell:set(Cell, 1)
    || Cell <- Cells].

step_all(N, M) ->
  Cells = all_cells(N, M),
  lists:foreach( fun egol_cell:step/1, Cells).

run_all(N,M) ->
  Cells = all_cells(N, M),
  lists:foreach( fun egol_cell:run/1, Cells).

stop_all(N,M) ->
  Cells = all_cells(N, M),
  lists:foreach( fun egol_cell:stop/1, Cells).



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
  [ io:format( "~p", [Value])
    || Value <- Values ],
  io:format("~n").
                 

all_cells(N, M) ->
  [ {X, Y}
    || X <- lists:seq(0, N-1),
       Y <- lists:seq(0, M-1) ].

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
  Full = [{0,0}, {1,0}, {2,0}, {2,1},{1,2}],
  fill_cells(Full);
test({print,T}) ->
  print(8,8,T);
test(step) ->
  step_all(8,8);
test(run) ->
  run_all(8,8);
test(stop) ->
  stop_all(8,8);
test(3) ->
  test(1),
  timer:sleep(10),
  test(2),
%  percept_profile:start("egol-profile.dat", [procs]),
  test(run),
  timer:sleep(15000),
  test(stop);
test(4) ->
  percept:profile("egol-profile.dat", {?MODULE, test, [3]}, [procs]).
%  percept_profile:stop().
  

  
  
