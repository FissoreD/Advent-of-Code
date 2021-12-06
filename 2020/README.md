# These are the solutions of 2020 Advent of Code in Python

Notice :
solution are made with the OCalm langauge
to compile the src file, you should be placed in src folder and run the command
_ocamlc -c lib.ml_ to compile and create the cmo file
Then :
 - open a terminal 
 - run _ocaml_ command
 - #load "lib.cmo";;
 - #load  "str.cma";;
 - #use "dayXX.ml";; (* To open a file *)
 - the function to call to run the main should be _dayXX_Y()_ where XX is the day and Y is the part [1 or 2] of the challange