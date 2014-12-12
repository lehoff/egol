-module(egol_time).

-export([init/0,
         stop/0,
         set/2,
         min/0,
         max/0,
         clear/1]).


init() ->
  ets:new(egol_time, [named_table, public]),
  ets:insert(egol_time, [{max,0}]).

stop() ->
  ets:delete(egol_time).

set(XY, Time) ->
  ets:insert(egol_time, {XY, Time}),
  case Time > max() of
    true ->
      ets:insert(egol_time, {max, Time});
    false ->
      ok
  end.



max() ->
  [{max, Max}] = ets:lookup(egol_time, max),
  Max.

clear(XY) ->
  ets:delete(egol_time, XY).

min() ->
  case ets:first(egol_time) of 
    '$end_of_table' ->
      0;
    First ->
      InitialMin = ets:lookup_element(egol_time, First, 2),
    ets:foldr( fun({_,T}, Min) ->
                   min(T, Min)
               end,
               InitialMin,
               egol_time)
  end.
   
