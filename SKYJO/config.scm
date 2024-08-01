; Motley Sundry :: Game Models :: SKYJO :: config.scm
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

; CONFIGURATION
(define *num-players* 4) ; Number of players in the game.
(define *num-games* 5) ; Number of games to simulate.

(define (get-player-strat id)
    (if (= id 0) strat-naive
    (if (= id 1) strat-naive
    (if (= id 2) strat-naive
    (if (= id 3) strat-naive
    (if (= id 4) strat-naive
    (if (= id 5) strat-naive
    (if (= id 6) strat-naive
    (if (= id 7) strat-naive
)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generally, no changes are needed below here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; GAME CONSTANTS
(define *min-rounds* 10)
(define *max-rounds* 50)


; DECK CONSTANTS
(define *deck* '#s8(
    -2 -2 -2 -2 -2
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12))
(define *deck-size* 150)
(define *deck-mean* 5)
(define *deck-median* 4)

; PLAYER CONSTANTS
(define *player-num-cards* 12)

; ROUND CONSTANTS
(define *min-players* 2)
(define *max-players* 8)
(define *round-max-plays* 150)
(define *round-min-plays* 5)

; SIMULATION CONSTANTS
(define *simulation-min-games* 5)
(define *simulation-max-games* 150)

; CONFIGURATION VALIDATION
(if (> *num-games* *simulation-max-games*)
    (begin
        (display (list "Too many simulation games:" *num-games* ))
        (exit 1)
    )
)
(if (< *num-games* *simulation-min-games* 5)
    (begin
        (display (list "Too few simulation games:" *num-games* ))
        (exit 1)
    )
)
(if (> *num-players* *max-players*)
    (begin
        (display (list "Too many players:" *num-players* ))
        (exit 1)
    )
)
(if (< *num-players* *min-players*)
    (begin
        (display (list "Too few players:" *num-players* ))
        (exit 1)
    )
)

