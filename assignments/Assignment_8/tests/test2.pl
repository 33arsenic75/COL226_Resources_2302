prereq(col100,col106).
prereq(col100,col202).
prereq(col100,col215).
prereq(ell101,col215).
prereq(col215,col216).
prereq(col106,col226).
prereq(col106,cop290).
prereq(col106,col331).
prereq(cop290,col331).
prereq(col106,col333).
prereq(col106,col334).
prereq(col216,col334).
prereq(col106,col341).
prereq(mtl106,col341).
prereq(col106,col351).
prereq(col202,col352).
overlap(col202,mtl180).
overlap(ell201,col215).
overlap(ell305,col216).
overlap(col226,col765).

prereq(A,B):-prereq(A,C),prereq(C,B), C\=A.
overlap(A,B):-overlap(B,A).

/* testcases    
prereq(col100,col226).
prereq(col100,col331).
prereq(col100,ell201).
prereq(A,B),prereq(B,col334).
*/