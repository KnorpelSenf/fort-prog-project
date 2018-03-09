% Utility:

% Addition on peano numbers
add(o, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% Generate a descending list with all peano numbers 
% from 0 to the first parameter
enum(o, []).
enum(s(X), [s(X)|Xs]) :- enum(X, Xs).

% Append the second list to the first to get the third list
append([], X, X).
append([T|H], X, [T|L]) :- append(H, X, L).

% The first list is a permutation of the second list
permutation([], []).
permutation([T|H], X) :-
  permutation(H, H1),
  append(L1, L2, H1),
  append(L1, [T], X1),
  append(X1, L2, X).

% Equality
=(X, X).

% Inequality
\=(X, Y) :- \+(=(X, Y)).

% Modeling n-queens-problem:

% Check if two queens are on different diagonals
% P is the diagonal distance
differentDiags(Q, Q1, P) :-
  add(Q1, P, Q1PP), \=(Q, Q1PP), % unterschiedliche Diagonale \
  add(Q, P, QPP), \=(QPP, Q1). % unterschiedliche Diagonale /

% Check if a specific queen is (diagonally) safe from all other queens
% P is the diagonal distance
safe(_, [], _).
safe(Q, [Q1|Qs], P) :- differentDiags(Q, Q1, P), safe(Q, Qs, s(P)).

% Check if all queens are (diagonally) safe
allSafe([]).
allSafe([Q|Qs]) :- safe(Q, Qs, s(o)), allSafe(Qs).

% Check if L is a valid solution for the n-queens-problem
% with size N as peano number
queens(N,L) :-
  enum(N, Q),
  permutation(Q, L),
  allSafe(L).

% Convert a number to peano representation
% The first parameter has to be known!
toPNum(0, o).
toPNum(N, s(L)) :- >(N, 0), is(N1, -(N, 1)), toPNum(N1, L).

% Check if the list has the length of the second parameter
length([], 0).
length([X|XS], N) :- length(XS, M), is(N, +(M, 1)).

% Get the number of solutions N for the n-queens-problem with size S
% Both NOT as a peano-number
numberOfSolutions(S, N) :-
  toPNum(S, SP),
  findall(X, queens(SP, X), L),
  length(L, N).

