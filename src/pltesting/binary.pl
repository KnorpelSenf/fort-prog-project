% Adding the first two binary numbers yields the third parameter
add(o, N, N).
add(N, o, N).
add(pos(M), pos(N), pos(K)) :- addP(M, N, K).

% Like add/3 for numbers > 0
addP(i, i, o(i)).
addP(i, o(N), i(N)).
addP(i, i(N), o(K)) :- addP(i, N, K).
addP(o(M), i, i(M)).
addP(o(M), o(N), o(K)) :- addP(M, N, K).
addP(o(M), i(N), i(K)) :- addP(M, N, K).
addP(i(M), i, o(K)) :- addP(i, M, K).
addP(i(M), o(N), i(K)) :- addP(M, N, K).
addP(i(M), i(N), o(K)) :- addP(i, M, J), addP(J, N, K).

% Subtracting the second binary numbers from the first yields the third parameter
sub(M, N, K) :- add(N, K, M).

% First binary number is less than the second
less(o, pos(_)).
less(pos(M), pos(N)) :- lessP(M, N).

% Like less/2 for numbers > 0
lessP(i, o(_)).
lessP(i, i(_)).
lessP(o(M), o(N)) :- lessP(M, N).
lessP(o(M), i(M)).
lessP(o(M), i(N)) :- lessP(M, N).
lessP(i(M), o(N)) :- lessP(M, N).
lessP(i(M), i(N)) :- lessP(M, N).

