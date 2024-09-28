#!/usr/bin/env bash
#
# Motley Sundry :: SKYJO-Card-Game-Model :: Scheme :: skyjo-scm-run.sh
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
#------------------------------------------------------
set -euo pipefail
IFS=$'\n\t'
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'
# shellcheck disable=SC2034
HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_SCM="$HERE/.."
cd "$ROOT_SCM" || exit
#=======================================================
source scripts/include.sh

print_usage(){
cat << USAGE
    Usage: $(basename "$0") [ repl | batch ]
    Runs the SKYJO simulation, the default is to compile and run the binary.
        Options:
            batch - Run the simulation in the interpreter.
            repl - Starts the interpreter REPL in the SKYJO directory.
                Load the source:    (load "load-skyjo.scm") (load-skyjo)
                Run the simulation: (run-skyjo 100)

USAGE
}

cd SKYJO

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
SRC+=('strat-level3.scm')
SRC+=('strat-level2.scm')
SRC+=('strat-level1.scm')
SRC+=('strat-cheat.scm')

# Check the arguments
if [[ $# -eq 1 ]]; then
    if [[ $1 = 'repl' ]]; then
        echo 'Load the source:    (load "load-skyjo.scm") (load-skyjo)'
        echo 'Run the simulation: (run-skyjo 100)'
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
