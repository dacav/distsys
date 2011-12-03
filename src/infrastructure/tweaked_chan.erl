-module(tweaked_chan).
-author("Giovanni Simoni").
-export([send/2, untweaked_send/3]).

% This function assumes the message with its envelope, namely a tuple
% {To, From, Msg}.
%
% Filters are functions which can either manipulate the message or send
% it. In the first case they have to return a new message with envelope,
% and the next filter will be executed; in the second case they have to
% return the sent message itself.
%
% If no filter sends the data, a regular sending will be achieved.
%
apply_filters (From, To, Msg, Filters) ->
    case Filters of
        [] -> erlang:send(To, {From, Msg});
        [Flt | Flts] ->
            case Flt(From, To, Msg) of
                {F, T, M} -> apply_filters(F, T, M, Flts);
                Msg -> ok;
                _ -> throw({badarg, invalid_filter})
            end
    end.

send (To, Msg) ->
    From = self(),
    case application:get_env(yuna, chan_filters) of
        {ok, Filters} -> apply_filters(From, To, Msg, Filters);
        undefined -> throw(config_fail)
    end.

untweaked_send (From, To, Msg) ->
    erlang:send(To, {From, Msg}).
