-module(egol_eqc).
 
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-record(state,
  { cell,
    id,
    dim,
    content,
    time=0,
    collector,
    waiting_on = undefined,
    neighbour_count = 0,
    neighbour_history = []
  }).


api_spec() ->
  #api_spec{
     language = erlang,
     modules  = [
                  #api_module{
                     name = egol_protocol,
                     functions = [ #api_fun{ name = query_content, arity = 2},
                                   #api_fun{ name = query_response, arity = 2}
                                 ]
                    }
                ]}.
  


initial_state() ->
  #state{}.

%% @doc Default generated property
-spec prop_cell() -> eqc:property().
prop_cell() ->
  ?SETUP(fun() -> 
             %% setup mocking here
             eqc_mocking:start_mocking(api_spec()),  
             fun() -> ok end
         end, 
  ?FORALL(Cmds, commands(?MODULE),
 %         ?IMPLIES(length(Cmds)>20,
          begin
            start(),
            {H, S, Res} = run_commands(?MODULE,Cmds),
            stop(S),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      Res == ok))
          end))
%)
.


start() ->
  egol_time:init(),
  egol_cell_mgr:start(),
  egol_cell_sup:start_link(),
  ok.

stop(_S) ->
  catch exit(whereis(egol_cell_sup), normal),
  catch exit(whereis(egol_cell_mgr), normal),
  egol_time:stop(),
  ok.



weight(_S, get) -> 1;
weight(_S, cell) -> 1;
weight(_S, flood_requery_response) -> 20;
weight(_S, _) -> 4. 
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell(XY, Dim, Content) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
%  io:format("cell STARTED~n"),
  Pid.

cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_pre(S, _) ->
  S#state.cell == undefined.


cell_next(S, Pid, [CellId, Dim, Content]) ->
  S#state{cell=Pid, id=CellId, dim=Dim, content=Content}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step(Pid, _Id, _Time) ->
  egol_cell:step(Pid),
  timer:sleep(100),
  egol_cell:collector(Pid).

step_args(S) ->
  [S#state.cell, S#state.id, S#state.time].

step_pre(S, [Pid, _, _]) ->
  S#state.cell /= undefined andalso
  S#state.cell == Pid andalso 
    S#state.waiting_on == undefined.

step_callouts(S, [_Pid, _XY, _Time ]) ->
  ?PAR(lists:duplicate(8, ?CALLOUT(egol_protocol, query_content, [?WILDCARD, S#state.time], ok))).

step_return(_S, _) ->
  ok.

step_next(S, Res, _Args) ->
%  io:format("step_next time:~p~n", [S#state.time]),
  S#state{waiting_on = egol_util:neighbours_at(S#state.time, 
                                               egol_util:neighbours(S#state.id, S#state.dim)),
          collector=Res}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Pid, Time) ->
  Res = egol_cell:get(Pid, Time),
%  io:format("get works~n"),
  Res.

get_args(S) ->
  [S#state.cell, S#state.time].

get_pre(S, _) ->
  S#state.cell /= undefined.

get_post(S, [_Pid, _Time], Res) ->
  eq(Res, S#state.content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_response(Pid, Resp) ->
  send_query_response(Pid, Resp).
  
query_response_args(#state{id=undefined}) ->
      [undefined, undefined];
query_response_args(#state{cell=Cell, waiting_on=undefined}) when is_pid(Cell) ->
      [undefined, undefined];
query_response_args(S) ->
  [S#state.collector, {{neighbour(S), S#state.time}, content()}].

query_response_pre(#state{waiting_on=undefined}, _ ) -> false;    
query_response_pre(S, [Collector, {{Neighbour, Time}, _}] ) ->
  Collector /= undefined andalso
  S#state.waiting_on /= [] andalso
  lists:member({Neighbour, Time}, S#state.waiting_on) andalso
    Time == S#state.time.

query_response_next(S, _Res, [_, {{Neighbour, Time}, Content}=Resp]) ->
  NC = S#state.neighbour_count + Content,
  case lists:delete({Neighbour, Time}, S#state.waiting_on) of
    [] ->
      timer:sleep(50),
 %     io:format("got ALL neighbour contents~n"),
      S#state{content=next_content(S#state.content, NC),
              time=Time+1,
              waiting_on=undefined,
              collector=undefined,
              neighbour_count=0,
              neighbour_history=S#state.neighbour_history ++ [Resp]};
    Wait ->
      S#state{waiting_on = Wait, neighbour_count = NC,
              neighbour_history=S#state.neighbour_history ++ [Resp]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flood_query_response(Pid, CellContents) ->
  lists:foreach( fun(CC) ->
                     send_query_response(Pid, CC)
                 end,
                 CellContents).

cell_contents(CellsAtT) ->
  [ cell_content(CellAtT, content()) 
    || CellAtT <- CellsAtT ].

cell_content(CellAtT, Content) ->
  {CellAtT, Content}.

flood_query_response_args(#state{waiting_on=[_]})  -> 
  [undefined, undefined];
flood_query_response_args(#state{waiting_on=Wait}=S) when Wait /= [], 
                                                          Wait /= undefined ->
  ?LET(CellContents, cell_contents(tl(Wait)),
       [S#state.collector, CellContents]);
flood_query_response_args(_) ->
  [undefined, undefined].

flood_query_response_pre(_, [undefined, _]) -> false;
flood_query_response_pre(#state{waiting_on=[_]}, _)  -> false;
flood_query_response_pre(#state{waiting_on=Wait}, _) when Wait /= [],
                                                         Wait /= undefined ->
  true;
flood_query_response_pre(_,_) ->
  false.

flood_query_response_next(S, _Res, [_Pid, CellContents]) ->
  Contents = [ C || {_, C} <- CellContents],
  NC = lists:sum(Contents),
  S#state{waiting_on=[hd(S#state.waiting_on)],
          neighbour_count=S#state.neighbour_count+NC,
         neighbour_history=S#state.neighbour_history ++ CellContents}.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
complete_step(Pid, CellContents) ->
  flood_query_response(Pid, tl(CellContents)),
  query_response(Pid, hd(CellContents)).

complete_step_args(#state{waiting_on=Wait}=S) when is_list(Wait) andalso
                                                   length(Wait) > 1 ->
  [S#state.collector, cell_contents(Wait)];
complete_step_args(_) ->
  [undefined, undefined].

complete_step_pre(#state{waiting_on=Wait}) when is_list(Wait) andalso
                                                length(Wait) > 1 ->
  true;
complete_step_pre(_) ->
  false.

complete_step_next(S, Res, [Pid, CellContents]) ->
  S2 = flood_query_response_next(S, Res, [Pid, tl(CellContents)]),
  query_response_next(S2, Res, [Pid, hd(CellContents)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% kill(Id, EndTime, NeighbourHistory) ->
%%   egol_cell:kill(Id),
%%   timer:sleep(100),
%%   Pid = egol_cell_mgr:lookup(Id),
%%   Collector = egol_cell:collector(Pid),
%%   drive_collector(Pid, Collector, 0, EndTime, NeighbourHistory),
%%   Pid.

%% %% go forward until the cell has progressed to max time
%% drive_collector(_Pid, _Collector, T, T, NeighbourHistory) ->
%%   ;
%% drive_collector(Pid, Collector, T, EndTime, NeighbourHistory) ->
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GENERATORS
cell_id_and_dim() ->
  ?LET({DimX, DimY}, {dim(),dim()},
       ?LET({X,Y}, {coord(DimX), coord(DimY)},
            {{X,Y},{DimX,DimY}})).

dim() ->
  ?SUCHTHAT(N, nat(), N>2).

coord(Max) ->
  choose(0, Max-1).

pos_int() ->
  ?LET(N, nat(), N+1).

content() ->
  choose(0,1).

neighbour(S) ->
  oneof(egol_util:neighbours(S#state.id, S#state.dim)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS

next_content(0,3) -> 1;
next_content(C, N) when N==2; N==3 -> C;
next_content(_,_) -> 0.
  
send_query_response(Pid, {{_XY,_T}, _C}=Resp) ->
  Pid ! {cell_content, Resp}.  
