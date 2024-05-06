fact(0, 1).
fact(X, Y) :- X > 0, Z = X - 1, fact(Z, W), Y =  W * X.

fib_rec(0,0).
fib_rec(1,1).
fib_rec(N,A):- N > 1 ,N1 = N - 1, N2 = N - 2, fib_rec(N1,B),fib_rec(N2,C), A = B + C.

fib_tail(N, Result) :- fib_tail_help(N, 0, 1, Result).

fib_tail_help(0, A, B, A).
fib_tail_help(N, A, B, Result) :- N > 0,
    NextN = N - 1,
    NextA = B,
    NextB = A + B,
    fib_tail_help(NextN, NextA, NextB, Result).

/*
fact(10,A).
fib_rec(7,A).
fib(20,A).
fib_tail(7,A).
fib_tail(15,A).
*/