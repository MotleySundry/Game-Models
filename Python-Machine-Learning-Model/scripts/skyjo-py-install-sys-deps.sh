#!/usr/bin/env bash
#
# Motley Sundry :: SKYJO-Card-Game-Model :: Python :: skyjo-py-install-deps.sh
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
ROOT_PY="$HERE/.."
cd "$ROOT_PY" || exit
#=======================================================

hash -r

if [ "$(uname)" == "Darwin" ]; then
    if ! command -v brew &> /dev/null
    then
        echo "Home brew is not installed, install it from here:"
        echo "https://brew.sh/"
        exit 1
    else
        brew update
        brew install git
        brew install python3
        brew install shellcheck
    fi

elif [ "$(uname)" == "Linux" ]; then
    if ! command -v apt &> /dev/null
    then
        echo "Apt is not installed!"
        exit 1
    else
        sudo apt update
        sudo apt install -y git
        sudo apt install -y python3
        sudo apt install -y shellcheck
    fi

else
    echo "Unrecognized system: $(uname)"
    exit 1
fi

if ! command -v code &> /dev/null
then
    echo "VSCode is not installed, install it from here:"
    echo "https://code.visualstudio.com/download"
    echo "Then add it to the shell path: <shift><command> p"
    echo "Search for 'shell' and run the add command."
    exit 1
fi

hash -r

echo "Set your git identity if you havent already:"
echo '    git config --global user.name "John Doe"'
echo '    git config --global user.email "johndoe@email.com"'
