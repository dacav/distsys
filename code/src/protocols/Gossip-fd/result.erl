-module(result).
-author("Giovanni Simoni").
-export([new/0, add/3, stats/1, count/1]).

-record(
    result, {
        choices = gb_trees:empty(),
        stats = counter_new()
    }
).

new () ->
    #result{}.

add (Pid, Value, Res = #result{ choices=Choices, stats=Stats } ) ->
    NewChoices =
        case gb_trees:lookup(Pid, Choices) of
            none -> gb_trees:insert(Pid, Value, Choices);
            {value, Value} -> Choices;
            _ -> error
        end,
    case NewChoices of
        error ->
            {error, Res};
        _ ->
            {ok, Res#result {
                     choices = NewChoices,
                     stats = counter_update(Value, Stats)
                 }
            }
    end.

stats (#result{ stats=Stats }) ->
    counter_digest(Stats).

count (Result) ->
    gb_trees:size(Result#result.choices).            

counter_new () ->
    gb_trees:empty().

counter_update (Value, Counter) ->
    case gb_trees:lookup(Value, Counter) of
        none ->
            gb_trees:insert(Value, 1, Counter);
        {value, N} ->
            gb_trees:update(Value, N + 1, Counter)
    end.

counter_digest (Counter) ->
    gb_trees:to_list(Counter).
