% A simple Prolog program
p(X, Z) :- q(X, Y), p(Y, Z).
p(X, X).

q(a, b).

