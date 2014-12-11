-module(egol_util).

-export([neighbours/2]).
-export([neighbours_at/2]).

-export([next_content/2]).

-export([format_cell_value/1]).



neighbours({X,Y}, {DimX, DimY}) ->
  [ {Xa rem DimX, Ya rem DimY} ||
    Xa <- lists:seq(X-1+DimX, X+1+DimX),
    Ya <- lists:seq(Y-1+DimY, Y+1+DimY),
    {Xa,Ya} /= {X+DimX,Y+DimY}].

neighbours_at(T, Neighbours) ->
  [ {N, T} || N <- Neighbours ].


next_content(1, 2) -> 1;
next_content(_, 3) -> 1;
next_content(_, _) -> 0.
  

format_cell_value(0) -> io:format(".");
format_cell_value(1) -> io:format("*").