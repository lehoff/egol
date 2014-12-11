-module( array2 ).
 
-export([create/1, 
         get/2, 
         set/3]).
 
create({X, Y}) -> 
  array:new( [{size, X}, {default, array:new( [{size, Y}, {default, 0}] )}] ).
 
get( {X, Y}, Array ) -> 
  array:get( Y, array:get(X, Array) ).
 
set({X, Y}, Value, Array ) ->
	Y_array = array:get( X, Array ),
	New_y_array = array:set( Y, Value, Y_array ),
	array:set( X, New_y_array, Array ).
 
 
