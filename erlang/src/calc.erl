-module(calc).
-export([add/2,subtract/2,multiply/2,divide/2,exp/2]).

add(A, B) -> A + B.

subtract(A, B) -> A - B.

multiply(A, B) -> A * B.

divide(A, B) -> A / B.

exp(A, B) -> math:pow(A, B).
