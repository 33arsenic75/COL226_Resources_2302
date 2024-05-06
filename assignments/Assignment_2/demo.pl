:- use_module(library(lists)).
num(N):-integer(N).
bool(t):-!.
bool(f):-!.
bool(_):-false.
var(X) :- \+ bool(X), \+ num(X).
mem(X,[]):- false.
mem(X,[X|R]):-!.
mem(X,[H|T]):-mem(X,T).

hastype(G, E1, intT):-num(E1).
hastype(G, E1, boolT):- bool(E1).
hastype(G, E1, T):- var(E1),mem((E1,T),G).

hastype(G,E1+E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,E1*E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,E1-E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
hastype(G,E1/E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).

hastype(G, E1=E2,boolT):- hastype(G,E1,X),hastype(G,E2,X).
hastype(G, E1 + E2 ,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
hastype(G,E1 ^ E2,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
hastype(G,E1 (**) E2,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
hastype(G, \E1 ,boolT):-hastype(G,E1,boolT).
