% Append the second list to the first to get the third list
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

% E is the last element of list L
last(L, E) :- append(_, [E], L).

% The second list is the reverse od the first
reverse([], []).
reverse([E|R], L) :- reverse(R, UR), append(UR, [E], L).

% E is a member of the list in the second parameter
member(E, [E|_]).
member(E, [_|R]) :- member(E,R).

% Check if a list is sorted
sorted([]).
sorted([_]).
sorted([E1|[E2|L]]) :- =<(E1, E2), sorted([E2|L]).

% The second list is a permutation of the first list
perm([], []).
perm(L, [E|R]) :- remove(E, L, LwithoutE), perm(LwithoutE, R).

% The last list is like the first list without the element E
% Removes E only once 
remove(E, [E|R], R).
remove(E, [F|R], [F|RwithoutE]) :- remove(E, R, RwithoutE).

% Check if the List S is a sorted permutation of L
sort(L, S) :- perm(L, S), sorted(S).

