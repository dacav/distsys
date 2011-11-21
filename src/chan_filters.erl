-module(chan_filters).
-author("Giovanni Simoni").
-export([hitman/1, random_deliver/3, random_uniform_deliver/2]).

-import(randel).

% -----------------------------------------------------------------------
% "Hitman" filter.
%
% The hitman filter randomly kills the sender process instead of sending
% its messages. It's pretty much like having a mad postman...
% -----------------------------------------------------------------------

hitman (Prob) when (0 < Prob) andalso (Prob < 1)  ->
    fun (F, T, M) ->
        Dead = random:uniform(),
        case Dead =< Prob of
            true -> exit(F, crashed), M;
            false -> {F, T, M}
        end
    end;

hitman (0) ->
    fun (F, T, M) -> {F, T, M} end;

hitman (1) ->
    fun (F, _, M) -> exit(F, crashed), M end.

% -----------------------------------------------------------------------
% "Random Deliver" filter.
%
% Uses the 'randel' module to achieve a randomly delayed deliver of a
% message (tought to be placed as last element of a filter chain).
% -----------------------------------------------------------------------

random_deliver (MinDelay, MaxDelay, DelayDist) ->
    Spec = randel:build_spec(MinDelay, MaxDelay, DelayDist),
    fun (F, T, M) -> randel:send_rand(Spec, T, {F, M}), M end.

random_uniform_deliver (MinDelay, MaxDelay) ->
    random_deliver(MinDelay, MaxDelay, fun random:uniform/0).
