-module(recursion).
-export([pascal/1]).


pascal_row(1) -> [1];
pascal_row(N) ->
  Prev = pascal_row(N - 1),
  lists:map(fun({A,B}) -> A + B end, lists:zip([0] ++ Prev, Prev ++ [0])).

pascal(N) ->
  lists:map( fun(R) -> pascal_row(R) end, lists:seq(1, N)).
