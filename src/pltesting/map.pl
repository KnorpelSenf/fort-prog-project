% Check if mapping the predicate P over the first list yields the second list
maplist(_, [], []).
maplist(P, [X|Xs], [Y|Ys]) :- call(P, X, Y), maplist(P, Xs, Ys).

