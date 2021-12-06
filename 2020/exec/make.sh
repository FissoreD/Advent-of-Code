#/bin/zsh

ML="src/$1.ml"
INP="input/$1.txt"
touch $ML $INP
[ -s $ML ] || echo "open Lib" > $ML
[ "$2" = "-o" ] && code $ML $INP
