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
    steps=0,
    collector,
    waiting_on = undefined,
    neighbour_count = 0,
    neighbour_history = [],
    kill_count=0,
    pending_query_content=[]
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
  ?SETUP( %%fun my_setup/0, 
fun() -> 
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

my_setup() ->
  start(),
  fun() -> stop end.
      

start() ->
  catch egol_time:stop(),
  egol_sup:start_link(),
  ok.

stop() ->
%%  print_regs(),
  catch exit(whereis(egol_sup), normal),
  catch egol_time:stop(),
  eqc_mocking:stop_mocking(),
  timer:sleep(100),
  ok.

print_regs() ->
  try
    CellSup = whereis(cell_sup),
    MgrSup  = whereis(mgr_sup),
    io:format("CellSup: ~p - MgrSup: ~p~n", [CellSup, MgrSup])
  catch
    _:_ ->
      io:format("unable to print regs~n")
  end.

stop(S) ->
  catch exit(S#state.cell, normal),
  catch exit(S#state.collector, normal),
  timer:sleep(100).

%% is_process_dead(S#state.cell),
%%   is_process_dead(S#state.collector).

%% is_process_dead(undefinded) ->
%%   ok;
%% is_process_dead(Pid) ->
%%   case is_process_alive(Pid) of
%%     true ->
%%       timer:sleep(1),
%%       is_process_dead(Pid);
%%     false ->
%%       ok
%%   end.

%%weight(_S, kill) -> 0;
weight(#state{id=undefined}, Cmd) when Cmd /= cell -> 0;
weight(#state{waiting_on=W}, step) when is_list(W) -> 0;
weight(_S, get) -> 1;
weight(_S, cell) -> 1;
weight(_S, query_response) -> 25;
weight(#state{waiting_on=[_]}, last_query_response) -> 100;
%weight(_S, complete_step) -> 0;
weight(_S, kill) -> 1; 
weight(_S, _) -> 2.
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell(XY, Dim, Content) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
  %io:format("cell STARTED ~p (~p/~p)~n", [Pid, XY, Dim]),
  Pid.

cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_pre(S, _) ->
  S#state.cell == undefined.

cell_post(_S, [_, _, _], Res) ->
  is_pid(Res) and erlang:is_process_alive(Res).


cell_next(S, Pid, [CellId, Dim, Content]) ->
  S#state{cell=Pid, id=CellId, dim=Dim, content=Content}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step(Pid, Id, _Time) ->
  %io:format("step for ~p/~p~n", [Pid, Id]),
  egol_cell:step(Id),
%  Res = egol_cell:collector(Id),
  Res = await_defined_collector(Id),
  %io:format("defined collector: ~p~n", [Res]),
  sync_collector(Res),
  Res.

sync_collector(Pid) ->
  Ref = make_ref(),
  Pid ! {sync_collector, self(), Ref},
  receive
    Ref ->
      timer:sleep(10)
  end.

await_defined_collector(Id) ->
  case egol_cell:collector(Id) of
    undefined ->
      timer:sleep(5),
      await_defined_collector(Id);
    Pid ->
      Pid
  end.
      
step_args(S) ->
  [S#state.cell, S#state.id, S#state.time].

step_pre(S, [Pid, Id, _]) ->
   %io:format("step_pre cell:~p, pid:~p waiting_on:~p~n",
%             [S#state.cell, Pid, S#state.waiting_on]), 
  S#state.id /= undefined andalso
%%  S#state.cell == Pid  andalso 
    S#state.id == Id andalso
    S#state.waiting_on == undefined.

step_callouts(S, [_Pid, _XY, _Time ]) ->
  step_callouts_for_time(S#state.time).

step_callouts_for_time(Time) ->
    ?PAR(lists:duplicate(8, ?CALLOUT(egol_protocol, query_content, [?WILDCARD, Time], ok))).

%% step_return(_S, _) ->
%%   ok.

step_next(S, Res, _Args) ->
  %io:format("step_next time:~p~n", [S#state.time]),
  S#state{waiting_on = egol_util:neighbours_at(S#state.time, 
                                               egol_util:neighbours(S#state.id, S#state.dim)),
          collector=Res,
          steps=S#state.steps+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Id, Time) ->
  Res = egol_cell:get(Id, Time),
%  %%io:format("get works~n"),
  Res.

get_args(S) ->
  [S#state.id, S#state.time].

get_pre(S, [Id, Time]) ->
  S#state.id /= undefined andalso
    S#state.id == Id andalso
    S#state.time == Time.

get_post(S, [_Id, _Time], Res) ->
  Res == S#state.content.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_response(CollectorPid, Resp) ->
  io:format("query_response(~p, ~p)~n", [CollectorPid, Resp]),
  send_query_response(CollectorPid, Resp).
  
query_response_args(#state{id=undefined}) ->
      [undefined, undefined];
query_response_args(#state{cell=Cell, waiting_on=undefined}) when is_pid(Cell) ->
      [undefined, undefined];
query_response_args(S) ->
  [S#state.collector, neighbour_response(S)].

neighbour_response(S) ->
  %%io:format("neighbour_response: ~p~n", [S]),
  ?LET(N, neighbour(S),
       case lists:keyfind({N, S#state.time}, 1, S#state.neighbour_history) of
         false ->
           {{N, S#state.time}, content()}; 
         Resp ->
           Resp
       end). 

query_response_pre(_S, [undefined, undefined]) -> false;
query_response_pre(#state{waiting_on=undefined}, _ ) -> false;    
query_response_pre(S, [Collector, {{Neighbour, Time}, _}] ) ->
  Collector /= undefined andalso
  S#state.waiting_on /= [] andalso
  lists:member({Neighbour, Time}, S#state.waiting_on) andalso
    Time == S#state.time.

query_response_next(S, _Res, [_, {{Neighbour, Time}, Content}=Resp]) ->
  NC = S#state.neighbour_count + Content,
  Wait = lists:delete({Neighbour, Time}, S#state.waiting_on),
  S#state{waiting_on = Wait, neighbour_count = NC,
          neighbour_history=S#state.neighbour_history ++ [Resp]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
last_query_response(CollectorPid, Resp, {Id, AwaitTime}) ->
  send_query_response(CollectorPid, Resp),
  await_time_change(Id, AwaitTime).

await_time_change(Id, AwaitTime) ->
  case egol_cell:time(Id) of
    AwaitTime ->
      ok;
    _ ->
      io:format("await_time_change id ~p~n", [Id]),
      io:format("await_time_change pid ~p/~p~n", 
                [egol_cell_mgr:lookup(Id), egol_cell:collector(Id)]),
      timer:sleep(5),
      await_time_change(Id, AwaitTime)
  end.


last_query_response_args(#state{id=undefined}) ->
  [undefined];
last_query_response_args(#state{waiting_on=undefined}) ->
  [undefined];
last_query_response_args(#state{waiting_on=W}) when length(W) /= 1 ->
  [undefined];
last_query_response_args(S) ->
  [S#state.collector, {hd(S#state.waiting_on), content()}, {S#state.id, S#state.time+1}].

cell_content(#state{id=Id, time=Time, content=Content}) ->
  {{Id, Time}, Content}.

last_query_response_pre(_, [undefined]) -> false;
last_query_response_pre(S, [_CollectorPid, _Resp, {AwaitId, _AwaitTime}]) ->
    length(S#state.waiting_on)==1 andalso
    S#state.id == AwaitId.

last_query_response_callouts(#state{pending_query_content=[]}, _) ->
  ?EMPTY;
last_query_response_callouts(S, _) ->
  ?PAR([cell_content_msg(NeighbourId, NeighbourTime, S#state.content)
        || {NeighbourId, NeighbourTime} <- S#state.pending_query_content]).

last_query_response_next(S, _Res, [_CollectorPid, {_, Content}=Resp, _AwaitIdTime]) ->
  NC = S#state.neighbour_count + Content,
  io:format("last_query_response_next NC=~p~n", [NC]),
  S#state{pending_query_content=[],
          content=next_content(S#state.content, NC),
          time=S#state.time+1,
          waiting_on=undefined,
          collector=undefined,
          neighbour_count=0,
          neighbour_history=S#state.neighbour_history ++ [Resp]}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_content(Id, Time) ->
  send_query_content(Id, Time),
  timer:sleep(50),
  ok.

query_content_args(S) ->
  [S#state.id, S#state.time].

query_content_pre(S, [Id, Time]) ->
  S#state.id /= undefined andalso
    S#state.id == Id andalso
    S#state.time == Time.
    

query_content_callouts(S, [Id, Time]) ->
  CellContent =  {cell_content, {{Id, Time}, S#state.content}},
  ?CALLOUT(egol_protocol, query_response, [?WILDCARD, CellContent], ok).

query_content_next(S, _Res, [_Id, _Time]) ->
  S.


cell_content_msg(Id, Time, Content) ->
  {cell_content, {{Id, Time}, Content}}.

cell_content_msg(S) ->
  cell_content_msg(S#state.id, S#state.time, S#state.content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% query_future(Id, _RequestId, Time) ->
%%   send_query_content(Id, Time),
%%   ok.

%% query_future_args(S) ->
%%   Pending = [ N || {N,_} <- S#state.pending_query_content ],
%%   ?LET(Neighbour, oneof(egol_util:neighbours(S#state.id, S#state.dim) -- Pending),
%%        [S#state.id, Neighbour, S#state.time+1]).

%% query_future_pre(S, [Id, _NeighbourId, Time]) ->
%%   S#state.id /= undefined andalso
%%     S#state.id == Id andalso
%%     S#state.time == (Time+1).

%% query_future_next(S, _Res, [_Id, NeighbourId, Time]) ->
%%   S#state{pending_query_content=
%%             S#state.pending_query_content++[{NeighbourId, Time}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill(Id, 0, _) ->
  try 
    OldPid = egol_cell_mgr:lookup(Id),
    %io:format("killing ~p~n", [OldPid]),
    egol_cell:kill(Id),
    NewPid = await_new_cell_pid(OldPid, Id),
    %io:format("started ~p~n", [NewPid]),
    NewPid
  catch
    E:M ->
      io:format("kill exception ~p:~p ~p~n", [E, M, erlang:get_stacktrace()])
  end;
kill(Id, EndTime, NeighbourHistory) ->
  try
    OldPid = egol_cell_mgr:lookup(Id),
    egol_cell:kill(Id),
    Pid = await_new_cell_pid(OldPid, Id),
    %%io:format("kill new_pid:~p~n", [Pid]),
    Collector = egol_cell:collector(Pid),
    %%io:format("new collector: ~p~n", [Collector]),
    drive_collector(Pid, Id, Collector, 0, EndTime, NeighbourHistory),
    Pid
  catch
    E:M ->
      io:format("kill exception ~p:~p ~p~n", [E, M, erlang:get_stacktrace()])
  end.

await_new_cell_pid(OldPid, Id) ->
  case egol_cell_mgr:lookup(Id) of
    NewPid when is_pid(NewPid) andalso
                NewPid /= OldPid ->
      NewPid;
    _ ->
      timer:sleep(5),
      await_new_cell_pid(OldPid, Id)
  end.
    

%% go forward until the cell has progressed to max time
drive_collector(_Pid, _Id, Collector, T, T, _NeighbourHistory) ->
  Collector;
%%send_all_query_responses(Collector, NeighbourHistory);
drive_collector(Pid, Id, Collector, T, EndTime, NeighbourHistory) ->
  %% @todo consider filtering out the CellContents not for this time step, though
  %% they should do no harm.
  ResponsesForTime = [ R 
                      || {{_,RTime},_}=R <- NeighbourHistory,
                         RTime == T ],
  send_all_query_responses(Collector, ResponsesForTime),
  await_time_change(Id, T),
  NewCollector = egol_cell:collector(Pid),
  drive_collector(Pid, Id, NewCollector, T+1, EndTime, NeighbourHistory).

send_all_query_responses(Collector, NeighbourHistory) ->
    lists:foreach(fun(CellContent) ->
                      send_query_response(Collector, CellContent)
                  end,
                  NeighbourHistory).

kill_args(#state{id=Id, time=Time, neighbour_history=NH}) ->
  [Id, Time, NH].

kill_pre(S, [Id, EndTime, _]) ->
  S#state.cell /= undefined andalso
    S#state.id == Id andalso 
    S#state.time == EndTime andalso
    S#state.kill_count<1.


kill_callouts(_S, [_Id, 0, _]) ->
  ?EMPTY;
kill_callouts(_S, [_Id, EndTime, _NH]) ->
  ?SEQ( [ step_callouts_for_time(T) 
          || T <- lists:seq(0, EndTime-1) ] ).

%% after a kill the cell will run up to the EndTime and then await a step before it
%% can continue since it has not received a step command to drive it forward, so
%% waiting_on has to be undefined.
kill_next(S, Pid, %{Pid, Collector}, 
          [_Id, _EndTime, _NH]) ->
  S#state{cell=Pid, collector=undefined, 
          waiting_on=undefined,
          neighbour_count=0,
          kill_count=S#state.kill_count+1}.
    


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
  
send_query_response(Pid, {{XY,T}, C}=Resp) ->
  try
    Pid ! cell_content_msg(XY,T,C)
  catch
    _:_ ->
      io:format("send_query_response (~p, ~p) failed~n", 
                [Pid, Resp])
  end.

send_query_content(Id, Time) ->
  Pid = egol_cell_mgr:lookup(Id),
  Pid ! {query_content, Time, self()}.
