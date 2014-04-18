-module(recursion).
-export([pascal/1]).


pascal_row(1) -> [1];
pascal_row(N) ->
  Prev = pascal_row(N - 1),
  lists:map(fun({A,B}) -> A + B end, lists:zip([0] ++ Prev, Prev ++ [0])).

row_to_string(R) ->
  String_list = lists:map(fun(I) -> integer_to_list(I) end, R),
  string:join(String_list, " " ).

pascal(N) ->
  Rows = lists:map( fun(R) -> pascal_row(R) end, lists:seq(1, N)),
  Data = string:join(lists:map(fun(R) -> row_to_string(R) end ,Rows), "\n"),
  file:write_file("pascal.txt", io_lib:fwrite("~s\n", [Data])).
