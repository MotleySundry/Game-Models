# Game-Models
This repository contains software for modeling games.

The models are written in Gambit Scheme.

## To Install Scheme Mac prerequisites:
* brew install gambit-scheme
* brew install gcc@13
* brew install shellcheck

## There are two strategies implemented:
* **Cheat** - Is allowed to break the rules and is mainly used to find lower bounds for the higher strategies.
* **Level 1** - It is the most basic strategy where the rules are followed and decisions are made randomly.

Look at the comments at the top of the strategy modules for  more details.

## This is a sample output from a run:

```

Simulation Id:        0
Number of Games:      10000
Rounds per Game:      3.2
Player Strategy:      #(Level-1 Cheat Level-1 Cheat)
Removed Columns Mean: #(.3 .27 .3 .26)
Penalty Points Mean:  #(2. 19.1 1.5 14.7)
Point Median:         #(127. 36. 103. 38.)
Point Mean:           #(107. 34. 103. 35.)
Point STD:            #(21. 18. 23. 18.)
Point Max:            #(175 121 175 111)
Point Min:            #(-15 -80 -91 -71)

```

## Command Line Usage:

```

Usage: skyjo.sh [ repl | batch ]
Runs the SKYJO simulation, the default is to compile and run the binary.
    Options:
        batch - Run the simulation in the interpreter.
        repl - Starts the interpreter REPL in the SKYJO directory.
            Load the source:    (load "load-skyjo.scm") (load-skyjo)
            Run the simulation: (run-skyjo 100)

```


 
