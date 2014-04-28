% Caleb Everett                                    The College of New Jersey
% Programming Languages                                  Assignment 7 Prolog

% Part 3: Graph Coloring
different(red, green).    different(green, red).
different(red, yellow).   different(yellow, red).
different(red, blue).     different(blue, red).
different(green, yellow). different(yellow, green).
different(green, blue).   different(blue, green).
different(yellow, blue).  different(blue, yellow).

coloring(A,B,C,D,E,F) :-
  different(A, B), different(B, A),
  different(A, C), different(C, A),
  different(A, D), different(D, A),
  different(B, F), different(F, B),
  different(B, C), different(C, B),
  different(C, D), different(D, C),
  different(C, E), different(E, C),
  different(C, F), different(F, C),
  different(D, E), different(E, D),
  different(E, F), different(F, E).

% vim: ft=prolog
