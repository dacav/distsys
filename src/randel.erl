-module(randel).
-author("Giovanni Simoni").
-export([send_rand/5, send_rand/4, exit_rand/5, exit_rand/4, kill_rand/4,
         kill_rand/3]).

% ------------------------------------------------------------------------
% Random delay for actions.
%
% Follows the same style defined in the 'timer' module, but triggering is
% defined by boundary on the time and a function yielding a value between
% 0 and 1.
%
% Note: random:uniform/0 is a good example of Dist function.
% ------------------------------------------------------------------------

random_time (Min, Max, Dist) ->
    trunc(Min + (Max - Min) * Dist()).

send_rand (Min, Max, Dist, To, Msg) ->
    timer:send_after(random_time(Min, Max, Dist), To, Msg).

send_rand (Min, Max, Dist, Msg) ->
    send_rand (Min, Max, Dist, self(), Msg).

exit_rand (Min, Max, Dist, Pid, Reason) ->
    timer:exit_after(random_time(Min, Max, Dist), Pid, Reason).

exit_rand (Min, Max, Dist, Reason) ->
    exit_rand(Min, Max, Dist, self(), Reason).

kill_rand (Min, Max, Dist, Pid) ->
    timer:kill_after(random_time(Min, Max, Dist), Pid).

kill_rand (Min, Max, Dist) ->
    kill_rand(Min, Max, Dist, self()).
