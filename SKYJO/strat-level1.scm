; Motley Sundry :: Game Models :: SKYJO :: strat-naive.scm
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

;
; === Steps for the Level1 strategy ===
;
; 1) On the first-round two-flip, open any two cards.
;    ---- Game Play ----
; 2) If the discard is lower than the highest open card then replace it.
; 3) If the discard is five or lower, then replace any hidden card.
; 4) Otherwise; Draw a card.
; 5) If the draw is lower than the highest card in the hand, exchange it.
; 6) Otherwise; discard it.

; The most basic strategy that follows the rules with random choices.
; Returns #f when the last card is turned over.
(define (strat-level1 player cmd)

    (cond
        ; Returns the string label of the strategy or #f on failure.
        ((= cmd *strat-cmd-get-label*) "Level-1")

        ; Returns #t if the play was executed or #f otherwise
        ((= cmd *strat-cmd-play-phase1*)
                (strat-naive-any-phase player))
        
        ; Returns #t if the play was executed or #f otherwise.
        ((= cmd *strat-cmd-play-phase2*)
                (strat-naive-any-phase player))

        ; Returns the sum of the cards if the flips were executed #f otherwise.
        ((= cmd *strat-cmd-flip-two*)
            (strat-naive-flip-two player))
        
        (else
            (display "Unknown fommand: ")
            (display cmd)
            (newline)
            (exit 1)))
)

; Returns #t if the a play was executed #f otherwise
(define (strat-naive-any-phase player)

    (define high-open-card (player-api-get-highest-open-card player))
    (define discard-value (player-api-get-discard-val player))
    (define hidden-card (player-api-random-hidden-card-id player))

        (cond
            ; Try replacing the highest open card with the discard
            ((and high-open-card (< discard-value (player-api-get-open-card-value player high-open-card)))
                (player-api-replace-card-from-discard! player high-open-card)                
                #t)
                    
            ; Try replacing the hidden card with the discard
            ((and hidden-card (<= discard-value *deck-median*))
                (player-api-replace-card-from-discard! player hidden-card)                
                #t)
                        
            (else
                    ; Draw a card
                    (let ((draw-value (player-api-draw-card player)))
                    (cond
                        ; Try replacing the highest open card with the draw
                        ((and high-open-card (< draw-value (player-api-get-open-card-value player high-open-card)))
                            (player-api-replace-card-with-draw-card! player high-open-card draw-value)
                            #t)

                        ; Try replacing the hidden card with the draw
                        ((and hidden-card (<= draw-value *deck-median*))
                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; Discard the draw
                        (else
                            (player-api-discard-draw-card! player draw-value 
                                (player-api-random-hidden-card-id player))
                            #t)))))                 
)

; Returns (card1 card2)
(define (strat-naive-flip-two player)
    (define card1 (random-integer *hand-num-cards*))
    (list 
        card1
        (random-integer-exclude *hand-num-cards* card1))
)