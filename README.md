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

Simulation Id:    0
Num Games:        10000
Num Rounds:       31781
Player Strat:     #(Level-1 Cheat Level-1 Cheat)
Removed columns:  #(2974 2517 2909 2858)
Penalty Points:   #(20441 191300 14918 146582)
Point Median:     #(83. 15. 105. 43.)
Point Mean:       #(107. 34. 103. 35.)
Point STD:        #(22. 18. 23. 18.)
Point Max:        #(167 136 164 107)
Point Min:        #(-25 -78 -12 -91)

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


 
