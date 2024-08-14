# Game-Models
This repository contains software for modeling games.

The models are written in Gambit Scheme.

## To Install Scheme Mac prerequisites:
* brew install gambit-scheme
* brew install gcc@13
* brew install shellcheck

# SKYJO Card Game

## There are two strategies implemented:
* **Cheat** - Is allowed to break the rules and is mainly used to find lower bounds for the higher strategies.
* **Level 1** - It is the most basic strategy where the rules are followed and decisions are made randomly.

Look at the comments at the top of the strategy modules for  more details.

### This is a sample output from a run:

```

Simulation Id:    0
Number of Games:  10000
Rounds Mean:      3.3
Rounds Max:       5
Rounds Min:       3

Player Strategy:      #(Level-1 Cheat Level-1 Cheat)
Removed Columns Mean: #(.53 .43 .5 .44)
Penalty Points Mean:  #(2. 13.1 1.4 10.1)
Point Median:         #(100. 32. 98. 43.)
Point Mean:           #(107. 30. 103. 32.)
Point STD:            #(20. 16. 21. 16.)
Point Max:            #(170 101 172 119)
Point Min:            #(0 -14 0 -15)

```

### Command Line Usage:

```

Usage: skyjo.sh [ repl | batch ]
Runs the SKYJO simulation, the default is to compile and run the binary.
    Options:
        batch - Run the simulation in the interpreter.
        repl - Starts the interpreter REPL in the SKYJO directory.
            Load the source:    (load "load-skyjo.scm") (load-skyjo)
            Run the simulation: (run-skyjo 100)

```


 
