; Motley Sundry :: Game Models :: SKYJO :: strat-level1.scm
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

; Returns #f when the last card is turned over.
;
; =====================================
; === Steps for the Level1 strategy ===
; =====================================
; The most basic strategy that follows the rules with random choices.

; 1) On the first-round two-flip, open any two cards.
;    ---- Game Play ----
; 2) If the discard is lower than the highest open card then replace it.
; 3) If the discard is 5 or lower, then replace any hidden card.
; 4) Othrwise; Draw a card.
; 5) If the draw is lower than the highest card in the hand, exchange it.
; 6) If the draw is lower than 5, exchange it with a hidden card.
; 7) Otherwise; discard it.

(define (strat-level1 player cmd)

    (cond
        ; Returns the string label of the strategy or #f on failure.
        ((= cmd *strat-cmd-get-label*) "Level-1")

        ; Returns #t if a play was executed or #f otherwise
        ((= cmd *strat-cmd-play-phase1*)
                (strat-level1-any-phase player))
        
        ; Returns #t if a play was executed or #f otherwise.
        ((= cmd *strat-cmd-play-phase2*)
                (strat-level1-any-phase player))

        ; Returns a list of the card ids or #f otherwise.
        ((= cmd *strat-cmd-flip-two*)
            (strat-level1-flip-two player))
        
        (else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

; Returns (card1 card2)
(define (strat-level1-flip-two player)
    ; 1) On the first-round two-flip, open any two cards.
    (define card1 (random-integer *hand-num-cards*))
    (list 
        card1
        (random-integer-exclude *hand-num-cards* card1))
)
; Returns #t if the a play was executed #f otherwise
(define (strat-level1-any-phase player)

    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define hidden-card (player-api-random-hidden-card-id player))

        (cond
            ; 2) If the discard is lower than the highest open card then replace it.
            ((and highest-open-card-val (< discard-value highest-open-card-val))
                (player-api-replace-card-from-discard! player highest-open-card-idx)                
                #t)
                    
            ; 3) If the discard is the 5 or lower, then replace any hidden card.
            ((and hidden-card (<= discard-value 5))
                (player-api-replace-card-from-discard! player hidden-card)                
                #t)
                        
            (else
                    ; 4) Othrwise; Draw a card.
                    (let ((draw-value (player-api-draw-card player)))
                    (cond
                        ; 5) If the draw is lower than the highest card in the hand, exchange it.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val))
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 6) If the draw is lower than 5, exchange it with a hidden card.
                        ((and hidden-card (<= draw-value 5))
                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; 7) Otherwise; discard it.
                        (else
                            (player-api-discard-draw-card! player draw-value 
                                (player-api-random-hidden-card-id player))
                            #t)))))                 
)
