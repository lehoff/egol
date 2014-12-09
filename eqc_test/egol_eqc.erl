-module(egol_eqc).
 
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").

-record(state,
  { cell,
    started=false,
    id,
    dim,
    content,
    time=0,
    neighbour_contents = []
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
%%             fun() -> ok end
             fun() -> application:stop(gproc)  end %% Teardown function
         end, 
  ?FORALL(Cmds, commands(?MODULE),
          begin
            start(),
            {H, S, Res} = run_commands(?MODULE,Cmds),
            stop(S),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            Res == ok)
          end)).


start() ->
  application:start(gproc),
  egol_cell_sup:start_link(),
  ok.

stop(S) ->
  catch exit(whereis(egol_cell_sup), normal),
%%  egol_cell:kill(S#state.id),
  ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cell

cell(XY, Dim, Content) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
  Pid.

cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_pre(S, _) ->
  S#state.cell == undefined.

cell_next(S, Pid, [CellId, Dim, Content]) ->
  S#state{cell=Pid, id=CellId, dim=Dim, content=Content, started=true}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% step

step(Pid) ->
  Res = egol_cell:step(Pid),
  timer:sleep(50),
  Res.

step_args(S) ->
  [S#state.cell].

step_pre(S, [Pid]) ->
  S#state.cell /= undefined andalso
  S#state.cell == Pid.

step_callouts(S, [_XY]) ->
  ?PAR(lists:duplicate(8, ?CALLOUT(egol_protocol, query_content, [?WILDCARD, S#state.time], ok))).

step_return(_S, _) ->
  ok.

step_next(S, _Res, _Args) ->
  S.



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
