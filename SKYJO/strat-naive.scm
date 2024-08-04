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
        ; Returns #f if the player opened their last card, #t otherwise.

        ((equal? cmd "get-label") "Naive")

        ((equal? cmd "play-phase1") 
            (if (strat-naive-any-phase player)
                (player-any-cards-hidden? player)
                (log-fatal "Player failed to make a play: play-phase1" player)))

        ((equal? cmd "play-phase2")
            (or 
                (strat-naive-any-phase player)
                (log-fatal "Player failed to make a play: play-phase1" player)))

        ((equal? cmd "flip-two")
            (strat-naive-flip-two player))
        
        (else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

; Returns #t if the a play was executed #f otherwise
(define (strat-naive-any-phase player)

    (define high-open-card (player-get-largest-open-card player))
    (define discard-value (deck-discard-value (player-get-deck player)))
    (define hidden-card (player-get-first-hidden-card player))

        (cond
            ; Try replacing the highest open card with the discard
            ((and high-open-card (< discard-value (player-get-card player high-open-card)))
                (player-replace-card-from-discard! player high-open-card)                
                #t)
                    
            ; Try replacing the hidden card with the discard
            ((and hidden-card (<= discard-value *deck-median*))
                (player-replace-card-from-discard! player hidden-card)                
                #t)
                        
            (else
                    ; Draw a card
                    (let ((draw-value (deck-pop-draw-pile! (player-get-deck player))))
                    (cond
                        ; Try replacing the highest open card with the draw
                        ((and high-open-card (< draw-value (player-get-card player high-open-card)))
                            (player-replace-card-with-value! player high-open-card draw-value)
                            #t)

                        ; Try replacing the hidden card with the draw
                        ((and hidden-card (<= draw-value *deck-median*))
                            (player-replace-card-with-value! player hidden-card draw-value)
                            #t)

                        ; Discard the draw
                        (else
                            (player-discard-card! player draw-value))))))                 
)

(define (strat-naive-flip-two player)
    
    (define card1 (random-integer *hand-num-cards*))
    (define card2 (random-integer-exclude *hand-num-cards* card1))

    (player-set-card-open! player card1)
    (player-set-card-open! player card2)

    (+ (player-get-card-value player card1) 
        (player-get-card-value player card2))
)