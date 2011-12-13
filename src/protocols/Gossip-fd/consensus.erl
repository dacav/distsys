-module(consensus).
-author("Giovanni Simoni").
-export([init/1]).

-record(cons, {
    id,
    id_cycle,
    round=0,
    phase=0,
    est=nil,
    est_from_c=nil,
    rec=ordsets:new()
}).

init ({I, N}) ->
    #cons{ id=I, id_cycle=N }.
