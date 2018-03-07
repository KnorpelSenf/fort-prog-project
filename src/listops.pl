% Definiere Praedikat member(E,L): erfuellt falls Element E in Liste L enthalten

% E ist erstes Element:
member(E,[E|_]).
% E ist in Restliste enthalten:
member(E,[_|R]) :- member(E,R).

% perm(L1,L2): L2 ist Permutation von L1
perm([],[]).
perm(L,[E|R]) :- remove(E,L,LwithoutE), perm(LwithoutE,R).

remove(E,[E|R],R).
remove(E,[F|R],[F|RwithoutE]) :- remove(E,R,RwithoutE).

% Zwei Listen konkatenieren:
append([],L,L).
append([E|R],L,[E|M]) :- append(R,L,M).

% Letztes Element:
last(L,E) :- append(_,[E],L).
