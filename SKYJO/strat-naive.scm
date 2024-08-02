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
(define (strat-naive player cmd)

    (cond
        ( (equal? cmd "play-phase1") 
            (strat-naive-phase-1 player)
            ; Any hidden cards remaining, if so continue phase 1. 
            (player-any-hidden-cards? player))

        ( (equal? cmd "play-phase2")
            (strat-naive-phase-2 player))

        ( (equal? cmd "flip-two")
            (strat-naive-flip-two player))
        
        ( else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

(define (strat-naive-phase-1 player)
    (if (or (try-discard-top? player)
        (try-draw-card? player))
            #t
            (begin
                (display "!!! strat-naive-phase-1: failed to make a move!")(newline)
                (exit 1)
            )
    )
)

; Try to swap the drawn card with an up card.
; Returns #t if successful
(define (try-draw-card? player)
    (let ((draw (deck-pop-draw-pile (player-get-deck player))))
        (and
            draw
            (or
                (and                    
                    (player-any-open-cards? player)
                    (try-to-replace-open-card? player  draw))
                (and
                    (player-any-hidden-cards? player)
                    (try-to-replace-hidden-card? player draw))
                (and
                    (deck-push-discard-pile (player-get-deck player) draw)
                    (player-any-hidden-cards? player)
                    (player-open-first-hidden-card player)
                ))))
)

; Try to swap the discard top with an up card.
; Returns #t if successful
(define (try-discard-top? player)
    (let ((disc (deck-discard-top (player-get-deck player))))
        (and 
            disc 
            (or
                (and                    
                    (player-any-open-cards? player)
                    (try-to-replace-open-card? player disc))
                (and
                    (player-any-hidden-cards? player)
                    (try-to-replace-hidden-card? player disc)))

            (deck-pop-draw-pile (player-get-deck player))
        )
    )
)


(define (try-to-replace-open-card? player value)
    #f
)

(define (try-to-replace-hidden-card? player value)
    #f
)

(define (strat-naive-phase2 player )
    (strat-naive-phase1 player game round)
)

(define (strat-naive-flip-two player)
    
    (define card1 (random-integer *player-num-cards*))
    (define card2 (random-integer-exclude *player-num-cards* card1))

    (player-open-card player card1)
    (player-open-card player card2)

    (+ (player-get-card player card1) (player-get-card player card2))
)