; Motley Sundry :: Game Models :: SKYJO :: load-skyjo.scm
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

; Loads all of the SKYJO source files into the REPL.
; 1) Run the shell command: skyjo.sh repl
; 2) Load the SKYJO source into the REPL:
;      (load "load-skyjo.scm") (load-skyjo)
; 3) RUN the simulation:
;      (run-skyjo 100)
;

(define (load-skyjo)

(load "repl.scm")
(load "../library.scm")
(load "../vector.scm")
(load "../vector2.scm")
(load "../f64vector.scm")
(load "../f64matrix.scm")
(load "config.scm")
(load "deck.scm")
(load "game.scm")
(load "hand.scm")
(load "player.scm")
(load "round.scm")
(load "simulation.scm")
(load "strat-cheat.scm")
(load "strat-level1.scm")

)
