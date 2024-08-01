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
(define (strat-naive player table sim-stats cmd)
    (if (equal? cmd "phase-1") 
        (begin
            (strat-naive-phase-1 player table sim-stats)
            ; Any hidden cards remaining, if so continue phase 1. 
            (player-any-hidden-cards? player)
        )

    (if (equal? cmd "phase-2")
        (strat-naive-phase-2 player table sim-stats)

    (if (equal? cmd "flip-two")
        (strat-naive-flip-two player table sim-stats)
        (begin
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))))
)

(define (strat-naive-phase-1 player table sim-stats)
    (if (or (try-discard-top? player table sim-stats)
        (try-draw-card? player table sim-stats))
            #t
            (begin
                (display "!!! strat-naive-phase-1: failed to make a move!")(newline)
                (display table)(newline)
                (exit 1)
            )
    )
)

; Try to swap the drawn card with an up card.
; Returns #t if successful
(define (try-draw-card? player table sim-stats)
    (let ((draw (table-pop-draw-pile table)))
        (and
            draw
            (or
                (and                    
                    (player-any-open-cards? player)
                    (try-to-replace-open-card? player table sim-stats disc))
                (and
                    (player-any-hidden-cards? player)
                    (try-to-replace-hidden-card? player table sim-stats disc))
                (and
                    (table-push-discard-pile table draw)
                    (player-any-hidden-cards? player)
                    (player-open-first-hidden-card player)
                ))))
)

; Try to swap the discard top with an up card.
; Returns #t if successful
(define (try-discard-top? player table sim-stats)
    (let ((disc (table-discard-top table)))
        (and 
            disc 
            (or
                (and                    
                    (player-any-open-cards? player)
                    (try-to-replace-open-card? player table sim-stats disc))
                (and
                    (player-any-hidden-cards? player)
                    (try-to-replace-hidden-card? player table sim-stats disc))

            (table-pop-draw-pile table))))
)


(define (try-to-replace-open-card? player table sim-stats value)
    #f
)

(define (try-to-replace-hidden-card? player table sim-stats value)
    #f
)

(define (strat-naive-phase-2 player table sim-stats)
    (strat-naive-phase-1 player table sim-stats)
)

(define (strat-naive-flip-two player table sim-stats)
    
    (define card1 (random-integer 12))
    (define card2 (random-integer-exclude 12 card1))
    
    (s8vector-set!(player-card-state player) card1 1)
    (s8vector-set!(player-card-state player) card2 1)

    (+(s8vector-ref(player-cards player) card1)
        (s8vector-ref(player-cards player) card2))
)