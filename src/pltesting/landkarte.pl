% Solve the coloring problem for this adjacency graph:
% (or an equivalent plane)

% 1----2
% |\  /|
% | \/ |
% | /\ |
% |/  \|
% 3----4

% Define available colors 
farbe(rot).
farbe(gelb).
farbe(gruen).
farbe(blau).

% Define a (not neccesarily valid) coloration
faerbung(L1, L2, L3, L4) :- farbe(L1), farbe(L2), farbe(L3), farbe(L4).

% Define inequality on colors
verschieden(rot, gelb).
verschieden(rot, gruen).
verschieden(rot, blau).
verschieden(gelb, rot).
verschieden(gelb, gruen).
verschieden(gelb, blau).
verschieden(gruen, rot).
verschieden(gruen, gelb).
verschieden(gruen, blau).
verschieden(blau, rot).
verschieden(blau, gelb).
verschieden(blau, gruen).

% Define a valid coloration by checking adjacent nodes
korrekteFaerbung(L1, L2, L3, L4) :-
  verschieden(L1, L2),
  verschieden(L1, L3),
  verschieden(L2, L3),
  verschieden(L2, L4),
  verschieden(L3, L4),
  verschieden(L1, L4).

% Define all valid solutions
loesung(L1, L2, L3, L4) :-
  faerbung(L1, L2, L3, L4),
  korrekteFaerbung(L1, L2, L3, L4).

