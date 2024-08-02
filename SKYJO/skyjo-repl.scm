; Motley Sundry :: Game Models :: SKYJO :: skyjo-repl.scm
; Copyright (C) 2024 Donald R Anderson
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(log-info "Starting SKYJO in repl" #f)

(random-source-randomize! default-random-source)

(define simulation (new-simulation 0))
(simulation-run simulation)

(log-info "Finished SKYJO in repl" #f)
