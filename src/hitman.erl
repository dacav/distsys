-module(hitman).
-author("Giovanni Simoni").
-export([filter_by_pid/2]).

maybe_kill (Prob, Targets, F, T, M) ->
    IsDoomed = gb_sets:is_element(F, Targets) andalso
               random:uniform() =< Prob,
    case IsDoomed of
    true -> exit(F, crashed), M;
    false -> {F, T, M}
    end.

% Note: probability of 0 or 1 are pointless!
filter_by_pid (Prob, PidList) when (0 < Prob) andalso (Prob < 1)  ->
    Targets = gb_sets:from_list(PidList),
    fun (F, T, M) -> maybe_kill(Prob, Targets, F, T, M) end.

