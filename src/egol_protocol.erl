%%% @doc The intra-cell communication is placed in a module so we can mock it when
%%%      testing. 

-module(egol_protocol).

-compile([{parse_transform, lager_transform}]).

-export([query_content/2,
         query_response/2]).




query_content(XY, Time) ->
  Pid = egol_cell_mgr:lookup(XY),
  Pid ! {query_content, Time, self()}.


query_response(Pid, Resp) ->
  Pid ! Resp.
