% vim: ft=prolog
% Caleb Everett                                    The College of New Jersey
% Programming Languages                                  Assignment 7 Prolog

% Part 4: recursion and Trees

fib(0, 0).
fib(1, 1).
fib(N, T) :-
  N > 1,
  N_1 is N - 1,
  N_2 is N - 2,
  fib(N_1, T_1),
  fib(N_2, T_2),
  T is T_1 + T_2.
