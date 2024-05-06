:- use_module(library(lists)).

% defining helper functions
mem(X,[]):- false.
mem(X,[X|R]):-!.
mem(X,[H|T]):-mem(X,T).

% integers are of type integer
hastype(G, intT(X) , intT):-!.

% boolean are of type boolean
hastype(G,boolT(X), boolT):-!.

% Self explanatory
hastype(G, varT(X), T):- mem((X,T),G).
hastype([(X,T)|_], varT(X), T):- !.

% Self explanatory
hastype([(X,intT),(Y,intT)|_], add(varT(X),varT(Y)),intT):-!.
hastype(G, add(E1,E2), intT ) :- hastype(G,E1,intT),hastype(G,E2,intT).

hastype([(X,intT),(Y,intT)|_], sub(varT(X),varT(Y)),intT):-!.
hastype(G, sub(E1,E2), intT ) :- hastype(G,E1,intT),hastype(G,E2,intT).

hastype([(X,intT),(Y,intT)|_], mul(varT(X),varT(Y)),intT):-!.
hastype(G, mul(E1,E2), intT ) :- hastype(G,E1,intT),hastype(G,E2,intT).

hastype([(X,intT),(Y,intT)|_], div(varT(X),varT(Y)),intT):-!.
hastype(G, div(E1,E2), intT ) :- hastype(G,E1,intT),hastype(G,E2,intT).

hastype([(X,boolT),(Y,boolT)|_], and(varT(X),varT(Y)),boolT):-!.
hastype(G, and(E1,E2), boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).

hastype([(X,boolT),(Y,boolT)|_], or(varT(X),varT(Y)),boolT):-!.
hastype(G, or(E1,E2), boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).

hastype([(X,boolT),(Y,boolT)|_], xor(varT(X),varT(Y)),boolT):-!.
hastype(G, xor(E1,E2), boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).

hastype([(X,boolT),(Y,boolT)|_], nor(varT(X),varT(Y)),boolT):-!.
hastype(G, nor(E1,E2), boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).

hastype([(X,boolT),(Y,boolT)|_], nand(varT(X),varT(Y)),boolT):-!.
hastype(G, nand(E1,E2), boolT) :- hastype(G,E1,boolT),hastype(G,E2,boolT).

hastype(G, not(E1), boolT) :- hastype(G, E1, boolT).

hastype([(X,Z),(Y,Z)|_], equal(varT(X),varT(Y)),boolT):- !.
hastype(G, equal(E1,E2), boolT) :- hastype(G,E1,X),hastype(G,E2,X).

hastype([(X,intT),(Y,intT)|_], greaterthan(varT(X),varT(Y)),boolT):-!.
hastype(G, greaterthan(E1,E2), boolT) :- hastype(G,E1,X),hastype(G,E2,X).

hastype([(X,intT),(Y,intT)|_], lessthan(varT(X),varT(Y)),boolT):-!.
hastype(G, lessthan(E1,E2), boolT) :- hastype(G,E1,X),hastype(G,E2,X).


% :- use_module(library(lists)).
% num(N):-integer(N).
% bool(t):-!.
% bool(f):-!.
% bool(_):-false.
% var(X) :- \+ bool(X), \+ num(X).
% mem(X,[]):- false.
% mem(X,[X|R]):-!.
% mem(X,[H|T]):-mem(X,T).

% hastype(G, E1, intT):-num(E1).
% hastype(G, E1, boolT):- bool(E1).
% hastype(G, E1, T):- var(E1),mem((E1,T),G).

% hastype(G,E1+E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
% hastype(G,E1*E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
% hastype(G,E1-E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).
% hastype(G,E1/E2,intT):-hastype(G,E1,intT),hastype(G,E2,intT).

% hastype(G, E1=E2,boolT):- hastype(G,E1,X),hastype(G,E2,X).
% hastype(G, E1 + E2 ,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
% hastype(G,E1 ^ E2,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
% hastype(G,E1 (**) E2,boolT):-hastype(G,E1,boolT),hastype(G,E2,boolT).
% hastype(G, \E1 ,boolT):-hastype(G,E1,boolT).
