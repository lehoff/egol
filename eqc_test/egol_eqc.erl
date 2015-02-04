-module(egol_eqc).
 
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-record(state,
  { id,
    dim,
    content,
    time=0,
    steps=0,
    waiting_on = undefined,
    neighbour_count = 0,
    neighbour_history = [],
    neighbours = [] :: [pid()],
    kill_count=0,
    pending_query_content=[]
  }).


api_spec() ->
  #api_spec{
     language = erlang,
     modules  = [
                  #api_module{
                     name = egol_protocol,
                     functions = [ #api_fun{ name = query_content, arity = 3},
                                   #api_fun{ name = query_response, arity = 2}                     
                                 ]
                    }
%% ,
%%                  #api_module{
%%                     name = egol_time,
%%                     functions = [ #api_fun{ name = max, arity = 0},
%%                                   #api_fun{ name = set, arity = 2},
%%                                   #api_fun{ name = clear, arity = 1},
%%                                   #api_fun{ name = init, arity = 0}]
%%                    }
                ]}.
  


initial_state() ->
  #state{}.

%% @doc Default generated property
-spec prop_cell() -> eqc:property().
prop_cell() ->
  ?SETUP( %%fun my_setup/0, 
fun() -> 
             %% setup mocking here
    %% eqc_mocking:start_mocking(api_spec()),
     egol_sup:start_link(),
    ok,
             fun() -> stop() end
         end, 
  with_parameter(default_process, worker,   %% THis is the dirty trick
   ?FORALL(Cmds, commands(?MODULE),
 %         ?IMPLIES(length(Cmds)>20,
          begin
%%            print("Cmds: ~p~n", [Cmds]),
            start(),
            eqc_mocking:start_mocking(api_spec()),
            {H, S, Res} = run_commands(?MODULE,Cmds),
            stop(S),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            aggregate(command_names(Cmds),
                                      Res == ok))
          end))
)
%)
.

%% my_setup() ->
%%   start(),
%%   fun() -> stop() end.
      

start() ->
  eqc_mocking:start_mocking(api_spec()),
  catch egol_time:stop(),
  egol_time:init(),
  %%egol_sup:start_link(),
  ok.


stop() ->
  %%  print_regs(),
  %catch exit(whereis(egol_sup), shutdown),
  catch egol_time:stop(),
  catch eqc_mocking:stop_mocking(),
  stop_egol_sup(),
  timer:sleep(100),
  ok.

stop_egol_sup() ->
  case whereis(egol_sup) of
    undefined -> 
      ok;
    Pid ->
      Ref = monitor(process, Pid),
      catch exit(Pid, shutdown),
      receive
        {'DOWN', Ref, process, Pid, _Reason} ->
          ok
      after 1000 ->
          print("egol_sup not going down~n"),
          error(exit_timeout)
      end
  end.


stop(S) ->
  eqc_mocking:stop_mocking(),
  catch case egol_cell_mgr:lookup(S#state.id) of
          undefined ->
            ok;
          Pid ->  
            Ref = monitor(process, Pid),
            CollectorPid = egol_cell:stop(S#state.id),
            await_death(Pid, Ref),
            print("CollectorPid: ~p~n", [CollectorPid]),
            exit(CollectorPid, stop),
            await_collector_death(CollectorPid),
            print("collector is dead~n")
        end,
  timer:sleep(50),
  [ catch exit(NPid, stop) || NPid <- S#state.neighbours ],
  %%stop_egol_sup(),
  %% timer:sleep(100),
  ok.

await_death(Pid, Ref) ->
  %% case erlang:is_process_alive(Pid) of
  %%   true ->
  %%     print("await death~n"),
  %%     timer:sleep(5),
  %%     await_death(Pid);
  %%   false ->
  %%     ok
  %% end.
  receive 
    {'DOWN', Ref, process, Pid, _Reason} ->
      print("await death DONE~n")
  end.

await_collector_death(Pid) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true ->
      timer:sleep(5),
      await_collector_death(Pid);
    false ->
      ok
  end;
await_collector_death(_) ->
  true.


%%weight(_S, kill) -> 0;
%% weight(#state{id=undefined}, Cmd) when Cmd /= cell -> 0;
%% weight(#state{id=undefined}, cell) -> 1;
%% weight(#state{}, query_content) -> 1; 
%% weight(_, query_future) -> 1;
%% weight(_, step) -> 1;
%% weight(_, _) -> 0;

%%
weight(#state{waiting_on=W}, step) when is_list(W) -> 0;
weight(_S, get) -> 1;
weight(_S, cell) -> 1;
weight(_S, query_response) -> 25;
weight(#state{waiting_on=[_]}, last_query_response) -> 100;
weight(_S, kill) -> 1; 
weight(_S, _) -> 2.
   

command_precondition_common(S, Cmd) ->
  S#state.id /= undefined orelse Cmd == cell.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cell(XY, Dim, Content) ->
  {ok, _Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
  print("cell STARTED (~p/~p)~n", [XY, Dim]),
  Neighbours = start_neighbours(XY, Dim),
%  monitor_dummies(Neighbours),
  %% print("cell neighbours: ~p~n", [Neighbours]),
  Neighbours.

start_neighbours(XY, Dim) ->
  Nids = egol_util:neighbours(XY, Dim),
  [ start_neighbour(Nid) || Nid <- Nids ].

start_neighbour(Nid) ->
  Pid = spawn( fun dummy_neigbour_loop/0 ),
  egol_cell_mgr:reg(Nid, Pid),
  print("start_neighbour(~p): ~p~n", [Nid, Pid]),
  %%{Nid, Pid},
  Pid.

dummy_neigbour_loop() ->
  receive
    _ -> dummy_neigbour_loop()
  end.
      
%% monitor_dummies(Ns) ->
%%   [ monitor(process, N) || N <- Ns ],
%%   spawn ( fun() -> monitor_loop() end).

%% monitor_loop() ->
%%   receive 
%%     Msg ->
%%       print("monitor_loop got ~p~n", [Msg]),
%%       monitor_loop()
%%   end.
  



cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_pre(S) ->
  S#state.id == undefined.

cell_post(_S, [_, _, _], _Neighbours) ->
%%  print("cell_post Neighbours: ~p~n", [Neighbours]),
  true.
%%  is_pid(Pid) and erlang:is_process_alive(Pid).

cell_next(S, Res, [CellId, Dim, Content]) ->
  S#state{id=CellId, dim=Dim, content=Content, neighbours=Res}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
step(Id, _Dim, _Time) ->
  print("step for ~p~n", [Id]),
  egol_cell:step(Id),
  await_collecting_status(Id).


await_collecting_status(Id) ->
  case egol_cell:collecting_status(Id) of
    undefined ->
      timer:sleep(5),
      await_collecting_status(Id);
    _ ->
      ok
  end.


      
step_args(S) ->
  [S#state.id, S#state.dim, S#state.time].

step_pre(S, [Id, Dim, Time]) ->
   %print("step_pre cell:~p, pid:~p waiting_on:~p~n",
%             [S#state.cell, Pid, S#state.waiting_on]), 
  S#state.id /= undefined andalso
    S#state.id == Id andalso
    S#state.dim == Dim andalso
    S#state.time == Time andalso
    S#state.waiting_on == undefined.

step_callouts(_S, [XY, Dim, Time ]) ->
  step_callouts_for_time(Time, egol_util:neighbours(XY, Dim), XY).

step_callouts_for_time(Time, _Neighbours, XY) ->
  Queries = lists:duplicate(8, ?CALLOUT(egol_protocol, query_content, [?WILDCARD, Time, XY], ok)),
%%  Monitorings = lists:duplicate(8, ?CALLOUT(egol_protocol, lookup, [?WILDCARD], self())),
  ?PAR(Queries).%++Monitorings).
  %% ?PAR([?CALLOUT(egol_protocol, query_content, [N, Time, XY], ok) 
  %%       || N <- Neighbours]).

step_next(S, _Res, _Args) ->
  %print("step_next time:~p~n", [S#state.time]),
  S#state{waiting_on = egol_util:neighbours(S#state.id, S#state.dim),
          steps=S#state.steps+1}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Id, Time) ->
  Res = egol_cell:get(Id, Time),
%  %%print("get works~n"),
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
query_response(Id, Resp) ->
  print("query_response(~p, ~p)~n", [Id, Resp]),
  send_query_response(Id, Resp).
  
query_response_args(S) ->
  [S#state.id, neighbour_response(S)].

neighbour_response(S) ->
  %%print("neighbour_response: ~p~n", [S]),
  ?LET(N, neighbour(S),
       case lists:keyfind({N, S#state.time}, 1, S#state.neighbour_history) of
         false ->
           {{N, S#state.time}, content()}; 
         Resp ->
           Resp
       end). 

query_response_pre(#state{waiting_on=undefined}) -> false;
query_response_pre(#state{waiting_on=W}) when length (W) =< 1 -> false; 
query_response_pre(_S) -> true.
  

%% query_response_pre(_S, [undefined, undefined]) -> false;
%% query_response_pre(#state{waiting_on=undefined}, _ ) -> false;
%% query_response_pre(#state{waiting_on=W}, _ ) when length(W) =< 1 -> false;
query_response_pre(S, [Id, {{Neighbour, Time}, _}] ) ->
  Id /= undefined andalso
  S#state.waiting_on /= [] andalso
  lists:member(Neighbour, S#state.waiting_on) andalso
    Time == S#state.time.

query_response_next(S, _Res, [_, {{Neighbour, _Time}, Content}=Resp]) ->
  NC = S#state.neighbour_count + Content,
  Wait = lists:delete(Neighbour, S#state.waiting_on),
  S#state{waiting_on = Wait, neighbour_count = NC,
          neighbour_history=S#state.neighbour_history ++ [Resp]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
last_query_response(Resp, {Id, AwaitTime}) ->
  print("last_query_response(~p, {~p, ~p})~n", [Resp, Id, AwaitTime]),
  send_query_response(Id, Resp),
  print("calling await_time_change(~p, ~p)~n", [Id, AwaitTime]),
  await_time_change(Id, AwaitTime).

await_time_change(Id, AwaitTime) ->
  case egol_cell:time(Id) of
    AwaitTime ->
      ok;
    _ ->
      %% print("await_time_change id ~p~n", [Id]),
      %% print("await_time_change pid ~p/~p~n", 
      %%           [egol_cell_mgr:lookup(Id), egol_cell:collector(Id)]),
      timer:sleep(5),
      await_time_change(Id, AwaitTime)
  end.


last_query_response_args(S) ->
  [{{hd(S#state.waiting_on), S#state.time}, content()}, {S#state.id, S#state.time+1}].

cell_content(#state{id=Id, time=Time, content=Content}) ->
  {{Id, Time}, Content}.

last_query_response_pre(#state{waiting_on=[_]}) -> true;
last_query_response_pre(_S) -> false. 
  


last_query_response_callouts(#state{pending_query_content=[]}, _) ->
  ?EMPTY;
last_query_response_callouts(S, [{_, Content}, _]) ->
  NC = S#state.neighbour_count + Content,
  NextContent = next_content(S#state.content, NC),
  ?PAR([?CALLOUT(egol_protocol, query_response, 
                 [?WILDCARD, cell_content_msg(S#state.id, NeighbourTime, NextContent)], ok)
        || {_NeighbourId, NeighbourTime} <- S#state.pending_query_content]).

last_query_response_next(S, _Res, [{_, Content}=Resp, _AwaitIdTime]) ->
  NC = S#state.neighbour_count + Content,
  print("last_query_response_next NC=~p~n", [NC]),
  S#state{pending_query_content=[],
          content=next_content(S#state.content, NC),
          time=S#state.time+1,
          waiting_on=undefined,
          neighbour_count=0,
          neighbour_history=S#state.neighbour_history ++ [Resp]}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
query_content(Id, Time) ->
  print("query_content(~p,~p)~n", [Id, Time]),
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
query_future(Id, RequestId, Time) ->
  print("query_future(~p, ~p, ~p)~n", [Id, RequestId, Time]),
  send_query_content(Id, Time),
  ok.

query_future_args(#state{id=undefined}) ->
  [undefined];
query_future_args(S) ->
%  print("query_future_args called~n"),
%  Pending = [ N || {N,_} <- S#state.pending_query_content ],
%  print("pending: ~p~n", [Pending]),
  ?LET(Neighbour, oneof(egol_util:neighbours(S#state.id, S#state.dim)),
       [S#state.id, Neighbour, S#state.time+1]).

query_future_pre(_S, [undefined]) -> false;
query_future_pre(S, [Id, _NeighbourId, Time]) ->
  %%print("query_future_pre ~p - ~p at ~p for ~p~n", [Id, Time, S#state.time, S#state.id]),
  S#state.id /= undefined andalso
    S#state.id == Id andalso
    (S#state.time+1) == Time.

query_future_next(S, _Res, [_Id, NeighbourId, Time]) ->
  S#state{pending_query_content=
            S#state.pending_query_content++[{NeighbourId, Time}]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill(Id, _Dim, 0, _, PendingQueryContent) ->
  print("kill(~p, _, 0, _, ~p)~n", [Id, PendingQueryContent]),
  try 
    OldPid = egol_cell_mgr:lookup(Id),
    %print("killing ~p~n", [OldPid]),
    egol_cell:kill(Id),
    await_new_cell_pid(OldPid, Id),
    %print("started ~p~n", [NewPid]),
    %resend_pending_query_content(Id, PendingQueryContent),
    ok
  catch
    E:M ->
      print("kill exception ~p:~p ~p~n", [E, M, erlang:get_stacktrace()])
  end;
kill(Id, _Dim, EndTime, NeighbourHistory, PendingQueryContent) ->
  print("kill(~p, _, ~p, ~p, ~p)~n", [Id, EndTime, NeighbourHistory, PendingQueryContent]),
  try
    OldPid = egol_cell_mgr:lookup(Id),
    egol_cell:kill(Id),
    await_new_cell_pid(OldPid, Id),
%    resend_pending_query_content(Id, PendingQueryContent),
    %%print("kill new_pid:~p~n", [Pid]),
    _ = egol_cell:collecting_status(Id),
    %%print("new collector: ~p~n", [Collector]),
    drive_collector(Id, 0, EndTime, NeighbourHistory),
    timer:sleep(50),
    ok
  catch
    E:M ->
      print("kill exception ~p:~p ~p~n", [E, M, erlang:get_stacktrace()])
  end.

resend_pending_query_content(Id, PendingQueryContent) ->
    %print("started ~p~n", [NewPid]),
    lists:foreach(fun({_NeighbourId, Time}) ->
                      send_query_content(Id, Time)
                  end,
                  PendingQueryContent).

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
drive_collector(_Id, T, T, _NeighbourHistory) ->
  ok;
%%send_all_query_responses(Collector, NeighbourHistory);
drive_collector(Id, T, EndTime, NeighbourHistory) ->
  print("drive_collector ~p~n", [NeighbourHistory]),
  ResponsesForTime = [ R 
                      || {{_,RTime},_}=R <- NeighbourHistory,
                         RTime == T ],
  print("drive_collector ~p~n", [ResponsesForTime]),
  send_all_query_responses(Id, ResponsesForTime),
  await_time_change(Id, T),
  _ = egol_cell:collecting_status(Id),
  drive_collector(Id, T+1, EndTime, NeighbourHistory).

send_all_query_responses(Id, NeighbourHistory) ->
    lists:foreach(fun(CellContent) ->
                      send_query_response(Id, CellContent)
                  end,
                  NeighbourHistory).

kill_args(#state{id=Id, dim=Dim, time=Time, 
                 neighbour_history=NH, pending_query_content=PendingQueryContent}) ->
  [Id, Dim, Time, NH, PendingQueryContent].

kill_pre(S, [Id, _Dim, EndTime, _, _]) ->
  S#state.id /= undefined andalso
    S#state.id == Id andalso 
    S#state.time == EndTime andalso
    S#state.kill_count<1.


kill_callouts(_S, [_Id, _Dim, 0, _, _]) ->
  ?EMPTY;
kill_callouts(_S, [Id, Dim, EndTime, _NH, _]) ->
  Neighbours = egol_util:neighbours(Id, Dim),
  StepCallouts = ?SEQ( [ step_callouts_for_time(T, Neighbours, Id) 
                         || T <- lists:seq(0, EndTime-1) ] ),
  %% PendingQueryResponses = ?PAR( [?CALLOUT( egol_protocol, query_response,
  %%                                [?WILDCARD, cell_content_msg(S#state.id, NeighbourTime, ?WILDCARD)], ok)
  %%                                || {_NeighbourId, NeighbourTime} <- S#state.pending_query_content ]),
  %% ?PAR(StepCallouts, PendingQueryResponses).
  StepCallouts.

%% after a kill the cell will run up to the EndTime and then await a step before it
%% can continue since it has not received a step command to drive it forward, so
%% waiting_on has to be undefined.
kill_next(S, _Pid, [_Id, _Dim, _EndTime, _NH, _]) ->
  S#state{waiting_on=undefined,
          pending_query_content = [],
          neighbour_count=0,
          kill_count=S#state.kill_count+1}.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
kill_neighbour(Nid, Id) ->
  Pid = egol_cell_mgr:lookup(Nid),
  print("kill_neighbour(~p,~p) : ~p~n", [Nid, Id, Pid]),
  ensure_death(Pid),
  print("kill_neighbour(~p) ~p done~n", [Nid, Pid]),
  NewPid = start_neighbour(Nid),
  timer:sleep(200),
  Status = egol_cell:collecting_status(Id),
  print("kill_neighbour status: ~p~n", [Status]),
  timer:sleep(200),
  NewPid.

ensure_death(Pid) ->
  Ref = monitor(process, Pid),
  exit(Pid, stop),
  receive
    {'DOWN', Ref, process, Pid, _Reason} -> ok
  end.
      

%% kill_neighbour_args(#state{waiting_on=undefined}) ->
%%   [undefined];
%% kill_neighbour_args(#state{neighbours=1}) ->
%%   [undefined];
kill_neighbour_args(S) ->
  %% Nids = [ Nid || {Nid, _T} <- S#state.waiting_on ],
  %% io:format("kill_neighbours_args: ~p  in ~p~n", [Nids, S#state.neighbours]),
  %% ?LET(Nid, oneof(Nids),
  %%      lists:keyfind(Nid, 1, S#state.neighbours)).
  [neighbour(S), S#state.id].
                  
kill_neighbour_pre(#state{waiting_on=undefined}) -> false;
kill_neighbour_pre(#state{waiting_on=W}) -> W /= [].
   
kill_neighbour_callouts(S, [Nid, _Id]) ->
%  Nids = [ N || {N, _T} <- S#state.waiting_on ],
  case lists:member(Nid, S#state.waiting_on) of 
    true ->
      ?CALLOUT(egol_protocol, query_content, [?WILDCARD, S#state.time, S#state.id], ok);
    false ->
      ?EMPTY
  end.

kill_neighbour_next(S, NewPid, _Args) ->   
   S#state{neighbours = [NewPid|S#state.neighbours]}.
  

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
  
send_query_response(Id, {{XY,T}, C}=Resp) ->
  try
%%    Pid ! cell_content_msg(XY,T,C)
    egol_cell:query_response(Id, cell_content_msg(XY, T, C))
  catch
    _:_ ->
      print("send_query_response (~p, ~p) failed~n", 
                [Id, Resp])
  end.

send_query_content(Id, Time) ->
  %% Pid = egol_cell_mgr:lookup(Id),
  %% Pid ! {query_content, Time, self()}.
  egol_cell:query_content(Id, Time, na).


print(Str) ->
  io:format(Str).
%  ok.

print(Str, Args) ->
  io:format(Str, Args).
  %ok.
