-module(basic_module).
-export([light_mixing/3,is_palindrome/1]).

light_mixing(0, 0, 0) -> black;
light_mixing(1, 1, 1) -> white;
light_mixing(1, 0, 0) -> red;
light_mixing(0, 1, 0) -> green;
light_mixing(0, 0, 1) -> blue;
light_mixing(1, 1, 0) -> magenta;
light_mixing(1, 0, 1) -> yellow;
light_mixing(0, 1, 1) -> cyan.

is_palindrome([_]) -> true;
is_palindrome([H|T]) ->
  case lists:last(T) of
    H -> is_palindrome(lists:sublist(T,length(T)-1));
    _ -> false
  end.
