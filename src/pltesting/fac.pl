% Compute the factorial F of N
fac(0, 1).
fac(N, F) :- >(N, 0), is(N1, -(N, 1)), fac(N1, F1), is(F, *(F1, N)).

