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
(define *num-players* 4)                ; Number of players in the game.
(define *num-games* 1000)            ; Number of games to simulate.
(define *cheating-allowed?* #f)         ; Allows calls outside the rules, strat-omnipotent
(define *write-ml-training-set?* #f)    ; If #t the simulation writes the ML training set.

(define (get-player-strat id)
    (if (= id 0) strat-level1
    (if (= id 1) strat-level2
    (if (= id 2) strat-level1
    (if (= id 3) strat-level2
    (if (= id 4) strat-level1
    (if (= id 5) strat-level2
    (if (= id 6) strat-level1
    (if (= id 7) strat-level2
)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generally, no changes are needed below here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ML TRAINING SET CONSTANTS
(define *ml-training-set-path* "/tmp/ml-training-set.txt")

; STRATEGY CONSTANTS
(define *strat-cmd-get-label*   0)
(define *strat-cmd-flip-two*    1)
(define *strat-cmd-play-phase1* 2)
(define *strat-cmd-play-phase2* 3)
(define *strat-look-ahead* 1)

; GAME CONSTANTS
(define *min-rounds* 10)
(define *max-rounds* 50)

; DECK CONSTANTS
(define *deck* '#(
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
(define *deck-sum* 760)
(define *deck-mean* 5)
(define *deck-median* 5)

; HAND CONSTANTS
(define *hand-num-cards* 12)
(define *card-state-hidden* 0)
(define *card-state-open* 1)
(define *card-state-removed* -1)

; ROUND CONSTANTS
(define *min-players* 2)
(define *max-players* 8)
(define *round-max-plays* 150)
(define *round-min-plays* 5)

; CONFIGURATION VALIDATION
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

