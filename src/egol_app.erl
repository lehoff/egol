-module(egol_app).

-behaviour(application).

-export([start/2, 
         stop/1]).

start(_StartType, _StartArgs) ->
  egol_sup:start_link().

stop(_) ->
  ok.
