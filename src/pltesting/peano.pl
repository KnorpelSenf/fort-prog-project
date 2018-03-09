% check if the parameter is a peano number
isPeano(o).
isPeano(s(N)) :- isPeano(N).

% succ(N,M) iff M is the successor of M
succ(N, s(N)).

% pred(N,M) iff M is the predecessor of M
pred(s(N), N).

% Addition on peano numbers
add(o, N, N) :- isPeano(N).
add(s(M), N, s(MN)) :- add(M, N, MN).

% Subtraction on peano numbers
sub(X, Y, Z) :- add(Y, Z, X).

% Multiplication on peano numbers
mult(o, _, o).
mult(s(M), N, P) :- mult(M, N, MN), add(MN, N, P).


% leq(X,Y) iff X is less or equal to Y
leq(o, _).
leq(s(M), s(N)) :- leq(M, N).

