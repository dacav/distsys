-module(node).
-author("Giovanni Simoni").

-export([behaviour_info/1]).

% -----------------------------------------------------------------------
% Defining behavior for implementing nodes.
% -----------------------------------------------------------------------

behaviour_info (callbacks) -> [
        {init, 1},
        {got_message, 3},
        {got_noise, 2},
        {meet, 3}
    ];
behaviour_info (_) -> undefined.

