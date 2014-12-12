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
    neighbour_count = 0
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
                    }%,
                 %% #api_module{
                 %%    name = gproc,
                 %%    functions = [ #api_fun{ name = where, arity=1},
                 %%                  #api_fun{ name = reg, arity=1} ]
                 %%   }
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
%%             fun() -> application:stop(gproc)  end %% Teardown function
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

stop(S) ->
  catch exit(whereis(egol_cell_sup), normal),
  catch exit(whereis(egol_cell_mgr), normal),
  egol_time:stop(),
%%  application:stop(gproc),
%%  egol_cell:kill(S#state.id),
  ok.



weight(_S, get) -> 1;
weight(_S, cell) -> 1;
weight(_S, flood_requery_response) -> 20;
weight(_S, _) -> 4. 
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell(XY, Dim, Content) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
  io:format("cell STARTED~n"),
  Pid.

cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_pre(S, _) ->
  S#state.cell == undefined.

cell_next(S, Pid, [CellId, Dim, Content]) ->
  S#state{cell=Pid, id=CellId, dim=Dim, content=Content}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step(Pid, Id, Time) ->
  %% case gproc:where(collector_name(Id, Time)) of
  %%   undefined ->
  %%     ok;
  %%   OldCollector ->
  %%     io:format("killed old collector~n"),
  %%     exit(OldCollector, kill)
  %% end,
  CollectorPid = egol_cell:step(Pid),
  timer:sleep(100),
  CollectorPid.
  %% timer:sleep(100),
  %% gproc:await(collector_name(Id, Time),100),
  %% case gproc:where(collector_name(Id, Time)) of
  %%   undefined ->
  %%     io:format("BUMMER!!! step has NOT started collector~n");
  %%   Collector ->
  %%     io:format("collector started after step:~p~n",[Collector]),
  %%     Collector
  %% end.
%%  Res.

collector_name(Id, Time) -> {n,l,{collector, Id, Time}}.

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

  S#state{waiting_on = egol_util:neighbours_at(S#state.time, 
                                               egol_util:neighbours(S#state.id, S#state.dim)),
          collector=Res}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Pid, Time) ->
  Res = egol_cell:get(Pid, Time),
  io:format("get works~n"),
  Res.

get_args(S) ->
  [S#state.cell, S#state.time].

get_pre(S, _) ->
  S#state.cell /= undefined.

get_post(S, [_Pid, _Time], Res) ->
  eq(Res, S#state.content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_response(Pid, XY, Time, Content) ->
%%  egol_protocol:query_response(Pid, {cell_content, {XY, Time}, Content}).
  Pid ! {cell_content, {{XY, Time}, Content}}.
  
query_response_args(#state{id=undefined}) ->
      [undefined, undefined, undefined, undefined];
query_response_args(#state{cell=Cell, waiting_on=undefined}) when is_pid(Cell) ->
      [undefined, undefined, undefined, undefined];
query_response_args(S) ->
  [S#state.collector, neighbour(S), S#state.time, content()];

query_response_args(S) ->
  try 
%%    {Collector,_} = gproc:await({n,l,{collector, S#state.id, S#state.time}},100),
    Collector =  gproc:where(collector_name(S#state.id, S#state.time)),
    case Collector of 
      undefined ->
        io:format("undefined collector~n");
      _ ->
        io:format("collector is running~n")
    end,

    [Collector, neighbour(S), S#state.time, content()]
  catch
    _:_ ->
      [undefined, undefined, undefined, undefined]
  end.

query_response_pre(#state{waiting_on=undefined}, _ ) -> false;    
query_response_pre(S, [Collector, Neighbour, Time, _] ) ->
  Collector /= undefined andalso
  S#state.waiting_on /= [] andalso
  lists:member({Neighbour, Time}, S#state.waiting_on) andalso
    Time == S#state.time.

query_response_next(S, _Res, [_, Neighbour, Time, Content]) ->
  NC = S#state.neighbour_count + Content,
  case lists:delete({Neighbour, Time}, S#state.waiting_on) of
    [] ->
      timer:sleep(50),
      io:format("got ALL neighbour contents~n"),
      S#state{content=next_content(S#state.content, NC),
              time=Time+1,
              waiting_on=undefined,
              collector=undefined,
              neighbour_count=0};
    Wait ->
      S#state{waiting_on = Wait, neighbour_count = NC}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flood_query_response(Pid, Cells) ->
  lists:foreach( fun(CC) ->
                     Pid ! CC
                 end,
                 Cells).

cell_contents(CellsAtT) ->
  [ cell_content(CellAtT, content()) 
    || CellAtT <- CellsAtT ].

cell_content(CellAtT, Content) ->
  {cell_content, {CellAtT, Content}}.

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
  Contents = [ C || {cell_content, {_, C}} <- CellContents],
  NC = lists:sum(Contents),
  S#state{waiting_on=[hd(S#state.waiting_on)],
         neighbour_count=S#state.neighbour_count+NC}.
  


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
  
