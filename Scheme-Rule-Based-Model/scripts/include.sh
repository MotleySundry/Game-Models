#!/usr/bin/env bash
#
# Motley Sundry :: Game Models :: include.sh
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

set -euo pipefail
IFS=$'\n\t'
export PS4='+(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]:+${FUNCNAME[0]}(): }'

# Args: base-name [src ...]
batch_gambit() 
{
    base=$1
    shift

    # TMP dir?
    if [[ ! -d "tmp" ]]; then
        mkdir tmp
    fi

    # Run REPL
    gsi "$@" "$base.scm" 
}

# Args: base-name [src ...]
repl_gambit() 
{
    # TMP dir?
    if [[ ! -d "tmp" ]]; then
        mkdir tmp
    fi

    # Run REPL
    gsi
}

# Args: base-name [src ...]
compile_gambit() 
{
    base=$1
    shift

    # TMP dir?
    if [[ ! -d "tmp" ]]; then
        mkdir tmp
    fi

    # Executable exists?
    if [[ ! -x "$base.bin" ]]; then
        gsc -o "$base.bin" -exe "$@" "$base.scm" 
        return
    fi

    # Source changed?
    for src in "$base.scm" "$@"; do
        if [[ "$src" -nt  "$base.bin" ]]; then
            rm "$base.bin"
            gsc -o "$base.bin" -exe "$@" "$base.scm" 
            return
        fi
    done
}