#!/usr/bin/env bash
#
# Motley Sundry :: SKYJO-Card-Game-Model :: Python :: INITIALIZE.sh
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
ROOT_PY="$HERE"
cd "$ROOT_PY" || exit
#=======================================================

./scripts/skyjo-py-validate.sh
./scripts/skyjo-py-install-sys-deps.sh
