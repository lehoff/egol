%%% @doc The intra-cell communication is placed in a module so we can mock it when
%%%      testing. 

-module(egol_protocol).

-compile([{parse_transform, lager_transform}]).

-export([query_content/3,
         query_response/2]).




query_content(XY, Time, FromXY) ->
  %% Pid = egol_cell_mgr:lookup(XY),
  %% Pid ! {query_content, Time, self()}.
  egol_cell:query_content(XY, Time, FromXY).

query_response(Cell, Resp) ->
  %% Pid ! Resp.
  egol_cell:query_response(Cell, Resp).
