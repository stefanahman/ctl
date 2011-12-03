% For Sicstus, uncomment line below: (needed for member/2)
:- use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).
% check(T, L, S, U, F)
%     T - The transitions in form of adjacency lists
%     L - The labeling
%     S - Current state
%     U - Currently recorded states
%     F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid. %
%(T,L),S|- F
%U
% To execute: consult(’your_file.pl’). verify(’input.txt’).
% Literals
check(_, L, S, [], X) :- 
	member([S,Srest], L),
	member(X,Srest).
	
check(_, L, S, [], neg(X)) :-
	member([S,Srest], L),
	not(member(X,Srest)).
	
% And
check(T, L, S, [], and(F,G)) :- 
	check(T, L, S, [], F),
	check(T, L, S, [], G).

% Or
check(T, L, S, [], or(F,_)) :-
	check(T, L, S, [], F).

check(T, L, S, [], or(_,G)) :-
	check(T, L, S, [], G).

% AX
check(T, L, S, [], ax(X)) :-
	member([S,Srest],T),
	acheck(T, L, Srest, [], X).

% EX
check(T, L, S, [], ex(X)) :-
	member([S,Srest],T),
	echeck(T, L, Srest, [], X).

% AG
check(_, _, S, U, ag(_)) :-
	member(S,U).
	
check(T, L, S, U, ag(X)) :-
	not(member(S,U)),
	member([S,Srest],T),
	check(T, L, S, [], X),
	acheck(T, L, Srest, [S|U], ag(X)).
	
% EG
check(_, _, S, U, eg(_)) :-
	member(S,U).
	
check(T, L, S, U, eg(X)) :-
	not(member(S,U)),
	member([S,Srest],T),
	check(T, L, S, [], X),
	echeck(T, L, Srest, [S|U], eg(X)).
	
% EF
check(T, L, S, U, ef(X)) :-
	not(member(S,U)),
	check(T, L, S, [], X).

check(T, L, S, U, ef(X)) :-
	not(member(S,U)),	
	member([S,Srest],T),
	echeck(T, L, Srest, [S|U], ef(X)).
	
% AF
check(T, L, S, U, af(X)) :-
	not(member(S,U)),
	check(T, L, S, [], X).
	
check(T, L, S, U, af(X)) :-
	not(member(S,U)),
	member([S,Srest],T),
	acheck(T, L, Srest, [S|U], af(X)).
	
%%% Helper functions %%%

acheck(_, _, [], _, _).
acheck(T, L, [S|Sl], U, X) :-
	check(T, L, S, U, X),
	acheck(T, L, Sl, U, X).

echeck(T, L, [S|_], U, X) :-
	check(T, L, S, U, X).
echeck(T, L, [_|Sl], U, X) :-
	echeck(T, L, Sl, U, X).

not(P) :- call(P), !, fail.
not(_).