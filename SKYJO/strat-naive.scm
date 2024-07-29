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
        (begin
            (strat-naive-draw-phase-1 player game sim-stats)
            (player-any-cards-down? player)
        )

    (if (equal? cmd "draw-phase-2")
        (strat-naive-draw-phase-2 player game sim-stats)

    (if (equal? cmd "flip-two")
        (strat-naive-flip-two player game sim-stats)
        (begin
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))))
)

(define (strat-naive-draw-phase-1 player game sim-stats)
    (if (or (try-discard-top? player game sim-stats)
        (try-draw-card? player game sim-stats))
            #t
            (begin
                (display "!!! Strat-naive: failed to make a move!")(newline)
                (display game)(newline)
                (exit 1)
            )
    )
)

; Try to swap the drawn card with an up card.
; Returns #t if successful
(define (try-draw-card? player game sim-stats)

    ; Try draw card to improve up cards
    (let (( draw-val (game-pop-draw-pile game))
        (idx (player-largest-up-card-idx player)))

        (let ((val (s8vector-ref (player-cards player) idx)))
            (if (and disc-val idx)
                (if (< draw-val val)
                    (begin
                        (s8vector-set!(player-cards player) idx val)
                        #t)
                    #f)
                #f)))
)

; Try to swap the discard top with an up card.
; Returns #t if successful
(define (try-discard-top? player game sim-stats)

    ; Try discard top to improve up cards
    (let (( disc-val (game-view-discard-top game))
        (idx (player-largest-up-card-idx player)))

        (let ((val (s8vector-ref (player-cards player) idx)))
            (if (and disc-val idx)
                (if (< disc-val val)
                    (begin
                        (s8vector-set!(player-cards player) idx (game-pop-draw-pile game))
                        #t)
                    #f)
                #f)))
)

(define (strat-naive-draw-phase-2 player game sim-stats)
    (strat-naive-draw-phase-1 player game sim-stats)
)

(define (strat-naive-flip-two player game sim-stats)
    
    (define card1 (random-integer 12))
    (define card2 (random-integer-exclude 12 card1))
    
    (s8vector-set!(player-card-up player) card1 1)
    (s8vector-set!(player-card-up player) card2 1)

    (+(s8vector-ref(player-cards player) card1)
        (s8vector-ref(player-cards player) card2))
)