:- use_module(library(lists)).
% Prerequiste Function 

% Defines set membership in recursive manner.
mem(A,[]):-false.
mem(H,[H|T]):-!.
mem(X,[H|T]):-mem(X,T).

% set_subtraction set_subtraction(A,B,C):- C is A-B,B is a single element not a set
set_subtraction([H|T], Set, Result) :- member(H, Set),!,set_subtraction(T, Set, Result).
set_subtraction([H|T], Set, [H|Result]) :-set_subtraction(T, Set, Result).

%For comparing 2 quantities, true when different
dif(A,A):-false.
dif(A,B):-!.

% nmem(A,B) if A is NOT member of B
nmem(A,B):- \+mem(A,B).

% del(A,B,C) deletes element A from B to get C
del(A,[A|B],C):-del(A,B,C).
del(A,[],[]):-!.
del(A,[B|C],[B|D]):-del(A,C,D).

% remdups(L1,L2) removes duplicates from L1 to get L2
remdups([A|B],[A|L2]):-del(A,B,C),remdups(C,L2).
remdups([],[]):-!.

% union(S1,S2,S3) S3 is union of S1 and S2.
unionI([ ], S2, S2) :- !.
unionI(S1, [ ], S1) :- !.
unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

%unionC(S1,S2,S3) checks whether S3 is intersection of S1,S2
unionC(S1,S2,S4):-unionC(S1,S2,S3),set_subtraction(S3,S4,[]).

% append(L1,L2,L3) L1 is appended in FRONT of L2 to get L3.
append([X|R],L2,[X|L3]):-append(R,L2,L3).
append([],L2,L2):-!.

% mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 
mapcons(X,[],[]):-!.
mapcons(X,[Y|R],[[X|Y]|Z]):-mapcons(X,R,Z).

% powerl(A,B) B is the power set of A.
powerI([X|R],P):-powerI(R,P1),mapcons(X,P1,P2),append(P2,P1,P).
powerI([],[[]]):-!.


% Part_a
% reflexive_transitive_closure((A, B), R, S)
reflexive_transitive_closure((A, B), R, S) :- 
    mem((A, B), R), % (A, B) is directly in R
    mem(A, S),     % A is in the set S
    mem(B, S).     % B is in set S


reflexive_transitive_closure((A, B), R, S) :-
    dif(A, B),            % A and B are different
    mem((A, C), R),       % (A, C) is in R
    set_subtraction(R,(X,A),R), % so that for (C,B) C is not A again and infinite recursion occurs. 
    reflexive_transitive_closure((C, B), R, S). % recursively check (C, B) in the closure

reflexive_transitive_closure((A, A), _, S) :-
    mem(A, S). % (A, A) is reflexive if A is in the set S
    
    
mem((A,B),reftran(R,S)):-reflexive_transitive_closure((A,B),R,S).

% reflexive_transitive_symetric_closure((A, B), R, S)
reflexive_transitive_symetric_closure((A, B), R, S):-
	reflexive_transitive_closure((A, B), R, S);  %either A->B exists or B->A in the directed graph representation of relation.
    reflexive_transitive_closure((B, A), R, S).

mem((A,B),reftransym(R,S)):-reflexive_transitive_symetric_closure((A,B),R,S).
    
% Part_b
% interl(S1,S2,S3) S3 implementation intersection of S1 and S2.
interI([A|B],S2,[A|S3]):-mem(A,S2),interI(B,S2,S3).
interI([A|B],S2,S3):-nmem(A,S2),interI(B,S2,S3).
interI([],S2,[]):-!.
interI(S1,S2,[X|S3]):-mem(X,S1),mem(X,S2),inter(S1,S2,S3).

%interC(S1,S2,S3) checks whether S3 is intersection of S1,S2
interC(S1,S2,S4):-interI(S1,S2,S3),set_subtraction(S3,S4,[]).

% diffl(S1,S2,S3) S3 is S2-S1
diffI([A|B],S2,S3):-mem(A,S2),diffI(B,S2,S3).
diffI([A|B],S2,[A|S3]):-nmem(A,S2),diffI(B,S2,S3).
diffI([],S2,[]):-!.

% cartesianl(S1, S2, S3) S3 is cartesian product of S1 and S2
cartesianI([A|B],S2,[L2|S3]):-mapcons(A,S2,L2),cartesianI(B,S2,S3).
cartesianI([],S2,[]):-!.


% test_cases
% 1. Check with sufficient examples  that unionI and powerI indeed implement union and power.
% unionI([a,b,c],[d,e,f],X). X=[a,b,c,d,e,f], both set doesn't everlap
% unionI([a,b,c,d,e,f],[b,c,d],X). X=[a,b,c,d,e,f], one set is subset of other
% unionI([a,b,c,d],[c,d,e,f],X). X=[a,b,c,d,e,f], both set partially everlap
% powerI([a],X). X=[[],[a]]
% powerI([a,b,c],X). X = [[a,b,c],[a,b],[a,c],[a],[b,c],[b],[c],[]]
% 2. Check that union does not have duplicates. 
% unionI([a,b,c,d],[c,d,e],X). X = [a,b,c,d,e], No repetation of any character
% unionI([a,b,c,d],[],X). X = [a,b,c,d]
% unionI([],[],X). X=[]
% 3. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  interI(S1, S2, S3) that implements intersection of two finite sets.  
% 4. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  diffI(S1, S2, S3) that implements set-difference of two finite sets.
% 5. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  cartesianI(S1, S2, S3) that implements cartesian of two finite sets.
% 6. Provide sufficient test cases examples to demonstrate your implementations are correct.
% Implemented these functions above
% interI([a,b,c],[d,e,f],X). X=[]. Nothing in common in both the set
% interI([a,b,c],[a,b,c,d,e],X). X=[a,b,c]. one set is subset of other
% interI([a,b,c,d],[c,d,e,f],X). X=[a,b,c,d,e,f], both set partially everlap
% interI([a,b,c],[a,b,c,d,e],X). X=[a,b,c]. one set is subset of other
% interI([],[c,d,e,f],X). X=[], one is empty set
% diffI([a,b,c],[d,e,f],X). X=[a,b,c]. Nothing in common in both the set
% diffI([a,b,c],[a,b,c,d,e],X). X=[]. one set is subset of other
% diffI([a,b,c,d],[c,d,e,f],X). X=[a,b], both set partially everlap
% diffI([a,b,c],[a,b,c,d,e],X). X=[]. S1 is subset of S2
% diffI([a,b,c,d,e],[a,b,c],X). X=[a,b,c]. S2 is subset of S1
% diffI([],[c,d,e,f],X). X=[], one is empty set
% cartesianI([a,b,c],[d,e,f],X). X = [[[a|d],[a|e],[a|f]],[[b|d],[b|e],[b|f]],[[c|d],[c|e],[c|f]]]. Nothing in common in both the set
% cartesianI([a,b,c],[a,b,c,d,e],X). X = [[[a|a],[a|b],[a|c],[a|d],[a|e]],[[b|a],[b|b],[b|c],[b|d],[b|e]],[[c|a],[c|b],[c|c],[c|d],[c|e]]]. one set is subset of other
% cartesianI([a,b,c,d],[c,d,e,f],X). X = [[[a|c],[a|d],[a|e],[a|f]],[[b|c],[b|d],[b|e],[b|f]],[[c|c],[c|d],[c|e],[c|f]],[[d|c],[d|d],[d|e],[d|f]]], both set partially everlap
% cartesianI([a,b],[a,b,c,d],X). X = [[[a|a],[a|b],[a|c],[a|d]],[[b|a],[b|b],[b|c],[b|d]]] S1 is subset of S2
% cartesianI([],[c,d,e,f],X). X=[], one is empty set
% 7. Suggest a way to check that the powersets obtained from the implementation of two different valid representations of a set (elements given in different order) are equal.
% We assume that the 2 given sets say P1 and P2 are power set of some set S1 and S2 respectively. Since P1 and P2 are sets of set, if we have a way to find the size of a
% set, we can combine all the members of P1 which are of size 1, which is guarantee S1 and similarly find S2. if S1 is subset of P2 and S2 is subset of P1, we can say P1 and P2 are powerset of same set
% If we can't find size of sets, but compare the size of 2 sets, we can find the largest element (which is itself a set) of P1 and P2. Let them be S1 and S2. If S1 is subset of S2 and vice versa we can say P1 and P2 are powerset of same set



