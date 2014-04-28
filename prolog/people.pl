% Caleb Everett                                    The College of New Jersey
% Programming Languages                                  Assignment 7 Prolog

% Part 2: Facts and Inferences
female(mary).
female(alex).
male(joe).
male(alex).
male(carl).

married(mary, carl).
married(carl, mary).

has_job(mary).

bio_major(joe).

watching_tv(joe).

no_sleep(alex).
no_sleep(Person) :- bio_major(Person), has_job(Person).
no_sleep(Person) :- bio_major(Person), has_kids(Person).
no_sleep(Person) :- has_job(Person), has_kids(Person).

has_kids(Person) :- male(Person), female(Wife), married(Person, Wife).
has_kids(Person) :- female(Person), male(Husband), married(Person, Husband).

buzy(Person) :- bio_major(Person).
buzy(Person) :- has_job(Person).
buzy(Person) :- has_kids(Person).

happy(Person) :- watching_tv(Person).
happy(Person) :- married(Person, _), \+ no_sleep(Person).

% vim: ft=prolog
