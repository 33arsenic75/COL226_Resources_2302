open Ast
open Lexer
open Parser

let parse_and_print str =
  let parsed_program = Parser.program (fun x -> Lexer.read x) (Lexing.from_string str) in
  let string_representation = string_of_program 0 parsed_program in
  Printf.printf "%s\n" string_representation

let () =
  parse_and_print "abhinav(123,456,csk(YOO,hiii)).";
  parse_and_print "father(john, bob) :- parent(bob).
                   father(john, alice) :- parent(alice),parent(BOB),thanos(SUCKS,226).";
  parse_and_print "fun(X) :- red(X), car(X).
                    fun(X) :- blue(X), bike(X).
                    car(vw_beatle).
                    car(ford_escort).
                    bike(harley_davidson).
                    red(vw_beatle).
                    red(ford_escort).
                    blue(harley_davidson).";
  parse_and_print "/* some comments
                      /* Author: 2022CS11596 */
                    */
                    likes(john, mary). % john likes marry
                    likes(john, trains).
                    likes(peter, fast_cars).
                    likes(Person1, Person2):- hobby(Person1, Hobby), hobby(Person2, Hobby).
                    hobby(john, trainspotting).
                    hobby(tim, sailing).
                    hobby(helen, trainspotting).
                    hobby(simon, sailing).";
  parse_and_print  "parent(john,paul).
                    parent(paul,tom).
                    parent(tom,mary).     
                    ancestor(X,Y):- parent(X,Y).
                    ancestor(X,Y):- parent(X,Z), ancestor(Z,Y).";
  parse_and_print  "adjacent(1,2).         adjacent(2,1). 
                    adjacent(1,3).         adjacent(3,1). 
                    adjacent(1,4).         adjacent(4,1). 
                    adjacent(1,5).         adjacent(5,1). 
                    adjacent(2,3).         adjacent(3,2). 
                    adjacent(2,4).         adjacent(4,2). 
                    adjacent(3,4).         adjacent(4,3). 
                    adjacent(4,5).         adjacent(5,4). 
                    color(1,red,a).        color(1,red,b). 
                    color(2,blue,a).       color(2,blue,b). 
                    color(3,green,a).      color(3,green,b). 
                    color(4,yellow,a).     color(4,blue,b). 
                    color(5,blue,a).       color(5,green,b).
                    conflict(Coloring) :- adjacent(X,Y), color(X,Color,Coloring), color(Y,Color,Coloring)."
  