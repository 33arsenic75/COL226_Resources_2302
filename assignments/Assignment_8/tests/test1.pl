team_players(chennai_super_kings, [ms_dhoni, ravindra_jadeja, shivam_dube, ruturaj_gaikwad]).
team_players(mumbai_indians, [rohit_sharma, ishan_kishan, jasprit_bumrah]).
team_players(royal_challengers_bangalore, [virat_kohli, ab_de_villiers, glenn_maxwell, faf_du_plessis]).
team_players(kolkata_knight_riders, [andre_russell, shreyas_iyer, sunil_narine, mitchell_starc]).
team_players(delhi_capitals, [rishabh_pant, prithvi_shaw, david_warner, ishant_sharma]).

member(X,[X|Y],true).
member(X,[Y|Z],A):-member(X,Z,A).
member(X,[],false).
team_member(T,P):-team_players(T,TL),member(P,TL,A),A=true.
team_role(T,R,P):-role(P,R),team_member(T,P).
role(A,all_rounder):-role(A,batsman),role(A,bowler).


role(ms_dhoni, wicketkeeper).
role(ms_dhoni, batsman).
role(ms_dhoni, captain).
role(ravindra_jadeja, batsman).
role(ravindra_jadeja, bowler).
role(shivam_dube, batsman).
role(shivam_dube, bowler).
role(ruturaj_gaikwad, batsman).
role(rohit_sharma, batsman).
role(rohit_sharma, captain).
role(ishan_kishan, wicketkeeper).
role(ishan_kishan, batsman).
role(jasprit_bumrah, bowler).
role(virat_kohli, batsman).
role(virat_kohli, captain).
role(ab_de_villiers, batsman).
role(glenn_maxwell, batsman).
role(faf_du_plessis, batsman).
role(andre_russell, batsman).
role(shreyas_iyer, batsman).
role(shreyas_iyer, captain).
role(sunil_narine, batsman).
role(sunil_narine, bowler).
role(mitchell_starc, bowler).
role(rishabh_pant, wicketkeeper).
role(rishabh_pant, batsman).
role(rishabh_pant, captain).
role(david_warner, batsman).
role(prithvi_shaw, batsman).
role(ishant_sharma, bowler).


/* testcases    
team_players(delhi_capitals,A).
team_member(A,jasprit_bumrah).
team_member(A,abhinavshripad).
team_role(A,wicketkeeper,ms_dhoni).
team_role(T1,R,rohit_sharma),team_role(T2,R,ravindra_jadeja).
*/