-module(conf).
-author("Giovanni Simoni").
-export([behaviour_info/1, get_default/1]).

% -----------------------------------------------------------------------
% Default configuration for modules exporting a configuration.
% -----------------------------------------------------------------------

behaviour_info(callbacks) -> [
        {default_conf, 0}
    ];
behaviour_info(_) -> undefined.

get_default (Module) ->
    try apply(Module, default_conf, []) of
        L when is_list(L) -> L;
        _ -> erlang:error(badarg, {Module, 'default not a list'})
    catch
        error:undef -> []
    end.

