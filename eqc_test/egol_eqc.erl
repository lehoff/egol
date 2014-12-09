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
    neighbour_contents = []
  }).


api_spec() ->
  #api_spec{
     language = erlang,
     modules  = [
                  %% #api_module{
                  %%    name = gproc_ps,
                  %%    functions = [ #api_fun{ name = subscribe, arity = 2},
                  %%                  #api_fun{ name = publish,   arity = 3}
                  %%                ]
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
             fun() -> application:stop(gproc) end %% Teardown function
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
  egol_cell:kill(S#state.id),
  ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cell

cell(XY, Dim, Content) ->
  {ok, Pid} = egol_cell_sup:start_cell(XY, Dim, Content),
  Pid.

cell_args(_S) ->
  ?LET({CellId, Dim}, cell_id_and_dim(),
       [CellId, Dim, content()]).

cell_next(S, Pid, [CellId, Dim, Content]) ->
  S#state{cell=Pid, id=CellId, dim=Dim, content=Content}.



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
