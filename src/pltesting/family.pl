% Equality
=(X, X).

% ehemann(F, M) :- iff M is F's husband
ehemann(christine, heinz).
ehemann(maria, fritz).
ehemann(monika, herbert).
ehemann(angelika, hubert).

% mutter(K, M) :- iff M is A's mother
mutter(herbert, christine).
mutter(angelika, christine).
mutter(hubert, maria).
mutter(susanne, monika ).
mutter(norbert, monika).
mutter(andreas, angelika).

% vater(K, V) :- iff V is K's father
vater(K, V) :- ehemann(M, V), mutter(K, M).

% elter(K, E) :- iff E is K's elter
elter(K, E) :- vater(K, E).
elter(K, E) :- mutter(K, E).

% grossvater(E, G) :- iff G is E's elter
grossvater(E, G) :- elter(E, F), vater(F, G).

% all grandfathers
grossvaeter(Gs) :- findall([E, G], grossvater(E, G), Gs).

% vorfahre(N, V) :- iff V is N's ancestor
vorfahre(N, V) :- vorfahre(N, V2), vorfahre(V2, V).
vorfahre(N, V) :- elter(N, V).

% vorfahre(S, P) :- iff S and P are siblings
geschwister(S, P) :- mutter(S, M), mutter(P,M), \+(=(P, S)).

