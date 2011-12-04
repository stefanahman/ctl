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

% p
check(_, L, S, [], X) :- 
	member([S,Srest], L),
	member(X,Srest).
	
% neg p	
check(_, L, S, [], neg(X)) :-
	member([S,Srest], L),
	not(member(X,Srest)).
	
% And
check(T, L, S, [], and(F,G)) :- 
	check(T, L, S, [], F),
	check(T, L, S, [], G).

% Or 1
check(T, L, S, [], or(F,_)) :-
	check(T, L, S, [], F).
% Or 2
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

% AG 1
check(_, _, S, U, ag(_)) :-
	member(S,U).
% AG 2
check(T, L, S, U, ag(X)) :-
	not(member(S,U)),
	member([S,Srest],T),
	check(T, L, S, [], X),
	acheck(T, L, Srest, [S|U], ag(X)).
	
% EG 1
check(_, _, S, U, eg(_)) :-
	member(S,U).
% EG 2	
check(T, L, S, U, eg(X)) :-
	not(member(S,U)),
	member([S,Srest],T),
	check(T, L, S, [], X),
	echeck(T, L, Srest, [S|U], eg(X)).
	
% EF 1
check(T, L, S, U, ef(X)) :-
	not(member(S,U)),
	check(T, L, S, [], X).
% EF 2
check(T, L, S, U, ef(X)) :-
	not(member(S,U)),	
	member([S,Srest],T),
	echeck(T, L, Srest, [S|U], ef(X)).
	
% AF 1
check(T, L, S, U, af(X)) :-
	not(member(S,U)),
	check(T, L, S, [], X).
% AF 2
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