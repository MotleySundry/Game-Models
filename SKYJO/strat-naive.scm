; Motley Sundry :: Game Models :: SKYJO :: strat-vaive.scm
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

; The most basic strategy that follows the rules with random choices.
; Returns #f when the last card is turned over
(define (strat-naive player game sim-stats cmd)
    (if (equal? cmd "draw-phase-1")
        #t
    (if (equal? cmd "draw-phase-2")
        #t
    (if (equal? cmd "flip-two")
        (strat-naive-flip-two player game sim-stats)
        (begin
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))))
)

(define (strat-naive-flip-two player game sim-stats)
    (s8vector-set!(player-card-up player) 0 1)
    (s8vector-set!(player-card-up player) 1 1)

    (+(s8vector-ref(player-cards player) 0)
        (s8vector-ref(player-cards player) 1))
)