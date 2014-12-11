-module(egol_array).

-export([start/2,
         run_until/2,
         run/2,
         print/1]).

-export([test/1]).

-export([get/2]).


-record(board,
        { matrix,
          dim,
          time=0}).

start(Dim, InitialCells) ->
  fill_cells(#board{ matrix = array2:create(Dim), dim = Dim},
             InitialCells).

run(Board, Time) ->
  Pid = running(Board),
  timer:sleep(Time),
  Pid ! {stop, self()},
  receive
    Res ->
      Res
  end.

running(Board) ->
  spawn(fun() -> running_loop(Board) end).
 
running_loop(Board) ->                 
  receive
    {stop, To} ->
      To ! Board
  after
    0 ->
      running_loop(step(Board))
  end.
          
    

run_until(Board, T) ->
  run_until(Board, 0, T+1).

run_until(Board, T, T) ->
  Board;
run_until(Board, T, EndTime) ->
  run_until(step(Board), T+1, EndTime).

set_cell(#board{matrix=M}=Board, XY, Content) ->
  Board#board{ matrix= array2:set(XY, Content, M)}.

fill_cells(Board, []) ->
  Board;
fill_cells(Board, [XY|Rest]) ->
  NewBoard = set_cell(Board, XY, 1),
  fill_cells(NewBoard, Rest).

get(#board{matrix=M}, XY) ->  
  array2:get(XY, M).

step(#board{matrix=M, dim=Dim, time=T}) ->
  NewBoard = start(Dim, []),
  evolve_board(NewBoard#board{time=T}, {0,0}, Dim, M).


evolve_board(#board{time=T}=Board, {0,Y}, {_DimX, DimY}, _M) when Y==DimY ->
  Board#board{time=T+1};
evolve_board(#board{matrix=NewM}=Board, {X,Y}=XY, {DimX, DimY}=Dim, M) when X<DimX, Y<DimY ->
  Content = compute_cell(XY, Dim, M),
  NewBoard = Board#board{ matrix=array2:set(XY, Content, NewM)}, 
  evolve_board(NewBoard, {X+1,Y}, Dim, M);
evolve_board(Board, {X,Y}, {DimX, _}=Dim, M) when X==DimX ->
  evolve_board(Board, {0, Y+1}, Dim, M).

compute_cell(XY, Dim, M) ->
  Neighbours = egol_util:neighbours(XY, Dim),
  egol_util:next_content(array2:get(XY,M), 
                         neighbour_count(M, Neighbours, 0)).

neighbour_count(_M, [], NC) ->
  NC;
neighbour_count(M, [XY|Rest], NC) ->
  neighbour_count(M, Rest, NC + array2:get(XY, M)).

                         
print(#board{matrix=M, dim={DimX, DimY}, time=T}) ->                            
  io:format("Time: ~p~n", [T]),
  print(M, DimY-1, DimX).

print(M, 0, DimX) ->
  print_row(M, 0, DimX);
print(M, Y, DimX) ->
  print_row(M, Y, DimX),
  print(M, Y-1, DimX).

print_row(M, Y, DimX) ->
  [ egol_util:format_cell_value(array2:get({X,Y}, M))
    || X <- lists:seq(0,DimX-1) ],
  io:format("~n").

test(1) ->
  start({8,8}, test(2));
test(2) ->
  [{0,0}, {1,0}, {2,0}, {2,1},{1,2}];
test(N) ->
  start({N,N}, test(2)).
