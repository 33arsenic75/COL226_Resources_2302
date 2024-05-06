edge(a, b).
edge(a, c).
edge(b, d).
edge(c, d).
edge(d, e).

path(X, Y) :-
    edge(X, Y). % If there is a direct edge between X and Y, there is a path.

path(X, Y) :-
    edge(X, Z),
    path(Z, Y). 

/*
path(a,e).
path(e,a).
edge(d,b).
*/