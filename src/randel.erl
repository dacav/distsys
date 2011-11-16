% ------------------------------------------------------------------------
% Random delay for actions.
%
% Follows the same style defined in the 'timer' module, but triggering is
% defined by boundary on the time and a function yielding a value between
% 0 and 1.
%
% Note: random:uniform/0 is a good example of Dist function.
% ------------------------------------------------------------------------

-module(randel).
-author("Giovanni Simoni").
-export([send_rand/3, send_rand/2, exit_rand/3, exit_rand/2, kill_rand/2,
         kill_rand/1]).

-include("randel.hrl").

random_time (Spec) ->
    Min = Spec#randspec.min,
    Max = Spec#randspec.max,
    Dist = Spec#randspec.dist,
    trunc(Min + (Max - Min) * Dist()).

% ------------------------------------------------------------------------
% Interface:
% ------------------------------------------------------------------------

send_rand (Spec, To, Msg) ->
    timer:send_after(random_time(Spec), To, Msg).

send_rand (Spec, Msg) ->
    send_rand (Spec, self(), Msg).

exit_rand (Spec, Pid, Reason) ->
    timer:exit_after(random_time(Spec), Pid, Reason).

exit_rand (Spec, Reason) ->
    exit_rand(Spec, self(), Reason).

kill_rand (Spec, Pid) ->
    timer:kill_after(random_time(Spec), Pid).

kill_rand (Spec) ->
    kill_rand(Spec, self()).
