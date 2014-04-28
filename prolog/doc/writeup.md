% Caleb Everett                                    The College of New Jersey
% Programming Languages                                  Assignment 7 Prolog

# Part 1: Research Questions

1. Describe the logic programming paradigm. What types of projects is it
  most useful for?

  Logic programming is to imperative programming as declaration is to
  command. The programmer defines things that are true and can ask what else
  can be inferred to be true from the given information.

  Logic programming is probably best for systems that are not procedural,
  and that involve analysis of relationships.

2. Name the three broad categories of objects in Prolog. Describe each
  category and the types of data best represented by each one.

  - *Atoms*: Objects who's definition is their own name.
  - *Structures*: functor + arity
  - *Variables*: Object who's value is the set of values that could make
    the funtor true.

3. Discuss the similarities and differences between compounds and data
  collections. Describe the two major data collections in Prolog.

  Both compounds and collections can be unified in a way that assigns each
  of the variables in the object to possible values. The unification
  procedure for compounds is defined in the functors body while data
  collections have fixed unification rules.

4. Explain the concept of homoiconicity and how it pertains to Prolog.

  Prolog code is a database of things that are true, and the file defining
  a Prolog program is similarly a set of things that are true.

5. What is  tail  recursion,  and why  should it  be  used? Demonstrate,
  with  an  example, why it should be used when possible.

  Tail recursion is when the recursive call is the last call to be executed
  in the current scope, so state of the current scope can be forgotten
  safely.

6. Can concurrency be handled in Prolog? Compare and contrast with other
  languages we have discussed in this class.

  The standard implementation of Prolog does not include concurrency.
  Because the programmer does not dictate how the program should execute
  concurrency in Prolog must be different than in other languages we have
  covered this semester. A Prolog implementation may be able to
  concurrency unify values automatically.

# Part 4: Clause Traces.

    fib(2, 99).

    2 > 1, N_1 is 2 - 1, N_2 is 2 - 2, fib(N_1, T_1), fib(N_2, T_2), T is T_1 + T_2
           N_1 is 1      N_2 is 0      T_1 is 1       T_2 is 0       99 is 0 + 1
    true,  true,         true,         true,          true,          false


    (1) fib(3, T).
      3 > 1, N_1 is 3 - 1, N_2 is 3 - 2, fib(N_1, T_1), fib(N_2, T_2), T is T_1 + T_2
             N_1 is 2      N_2 is 1      (2) T_1 is 1   (3) T_2 is 1   T is 1 + 1
      true,  true,         true,         true,          true,          false

      T = 2

    (2) fib(2, T).
      2 > 1, N_1 is 2 - 1, N_2 is 2 - 2, fib(N_1, T_1), fib(N_2, T_2), T is T_1 + T_2
             N_1 is 1      N_2 is 0      T_1 is 1       T_2 is 0       T is 0 + 1
      true,  true,         true,         true,          true,          true

      T = 1

    (2) fib(1, T).
      fib(1, 1)
      T is 1

      T = 1
