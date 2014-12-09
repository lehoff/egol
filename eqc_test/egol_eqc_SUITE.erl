-module(egol_eqc_SUITE).
 
-compile(export_all).

-include_lib("eqc/include/eqc_ct.hrl").

all() -> [check_prop_cell].

check_prop_cell(_) ->
    ?quickcheck((egol_eqc:prop_cell())).

