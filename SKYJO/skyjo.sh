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
    $(basename "$0") [ repl ]

USAGE
}

# Check the arguments
if [[ $# -eq 1 ]]; then
    if [[ $1 = 'repl' ]]; then
        repl_gambit skyjo-repl ../library.scm config.scm deck.scm player.scm game.scm round.scm simulation.scm strat-naive.scm 
        exit 1
    else
        usage
    fi
fi

build_gambit skyjo ../library.scm config.scm deck.scm player.scm game.scm round.scm simulation.scm strat-naive.scm 
./skyjo.bin "$@"
