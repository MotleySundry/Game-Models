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

Simulation Id: 0
Num Games:     10000
Num Rounds:    30640
Player Strat:  #(Level-1 Cheat Level-1 Cheat)
Removed Cols:  #(2333 2345 2010 2367)
Point Median:  #(108. 29. 89. 19.)
Point Mean:    #(108. 40. 105. 41.)
Point STD:     #(22. 20. 23. 19.)
Point Max:     #(178 129 179 121)
Point Min:     #(-17 -86 -78 -69)

```




 
