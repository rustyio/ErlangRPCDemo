-module(sequence).

-export([
         sequence/1
        ]).

sequence(N) when is_integer(N), N > 0->
    List = [list_to_binary(integer_to_list(X)) || X <- lists:seq(1, N)],
    {ok, List};
sequence(N) when is_integer(N) ->
    {error, {invalid_argument, N}};
sequence(S) when is_list(S) ->
    try
        sequence(list_to_integer(S))
    catch
        _Type:Msg ->
            {error, Msg}
    end.
