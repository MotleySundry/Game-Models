#!/usr/bin/env bash
#
# Motley Sundry :: Game Models :: SKYJO :: skyjo.sh
# Copyright (C) 2024 Donald R Anderson
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source ../include.sh

print_usage(){
cat << USAGE
    Usage: $(basename "$0") [ repl | batch ]
    Run the SKYJO simulation, the default is compile and run the binary.
        Options:
        repl - Load the source into the REPL for interactive debugging.
        batch - Run the simulation in the interpreter.

USAGE
}

SRC=()
SRC+=('../library.scm')
SRC+=('../f64matrix.scm')
SRC+=('../f64vector.scm')
SRC+=('../vector.scm')
SRC+=('../vector2.scm')
SRC+=('config.scm')
SRC+=('deck.scm')
SRC+=('hand.scm')
SRC+=('player.scm') 
SRC+=('game.scm')
SRC+=('round.scm')
SRC+=('simulation.scm') 
SRC+=('strat-level1.scm')
SRC+=('strat-cheat.scm')

# Check the arguments
if [[ $# -eq 1 ]]; then
    if [[ $1 = 'repl' ]]; then
        repl_gambit
        exit 0
    elif [[ $1 = 'batch' ]]; then
        batch_gambit skyjo "${SRC[@]}"
        exit 0
    else
        print_usage
        echo "!!! Invalid argument: $1"
        exit 1
    fi
fi

compile_gambit skyjo "${SRC[@]}"
./skyjo.bin "$@"
