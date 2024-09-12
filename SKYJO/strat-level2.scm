; Motley Sundry :: Game Models :: SKYJO :: strat-level2.scm
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
; === Steps for the Level2 strategy ===
; The second tier strategy that follows the rules and makes some round-level strategic decisions.
;
; ######################
; ###### PHASE 1 #######
; ######################
; 1) On the first-round two-flip, open any two cards in separate columns.

;    ---- If you have more than one hidden card (BASE-PLAY) ----
; 2) Using the discard complete a matching column if possible.
; 3) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 4) If the discard is 5 or lower, then replace any hidden card.
; 5) Otherwise; draw a card.
; 6) Using the drawn card complete a matching column if possible
; 7) If the draw card is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 8) If the draw card is 5 or lower, then replace any hidden card.
; 9) Otherwise; discard it.

;    ---- If you have only one hidden card (END-PLAY) ----
; 10) Estimate your hand value, by adding all the open cards plus 5 for the hidden card.
; 11) Estimate your opponents hand values, by adding up their open cards plus 5 points for each hidden card.
;
;    ---- If you your hand is low relative to the other players ----
; 12) Using the discard complete a matching column if possible.
; 13) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 14) If the discard is 5 or lower, then replace any hidden card.
; 15) Otherwise, draw a card 
; 16) Using the drawn card complete the column with the hidden card if possible 
; 17) If the drawn card is 5 or lower replace the highest open card greater than 5 otherwise.
; 18) If the drawn card is 5 or lower replace the hidden card.
; 19) If it is lower than the highest open card replace it.
; 20) Otherwise discard it.

;    ---- Otherwise do not replace the hidden card ----
; 21) If the discard is 5 or lower, replace: the highest open card.
; 22) Draw a card and replace the highest open card.

; ######################
; ###### PHASE 2 #######
; ######################
; --> BASE-PLAY

(define (strat-level2 player cmd)

    (cond
        ; Returns the string label of the strategy or #f on failure.
        ((= cmd *strat-cmd-get-label*) "Level-2")

        ; Returns #t if a play was executed or #f otherwise
        ((= cmd *strat-cmd-play-phase1*)
                (strat-level2-phase1 player))
        
        ; Returns #t if a play was executed or #f otherwise.
        ((= cmd *strat-cmd-play-phase2*)
                (strat-level2-phase2 player))

        ; Returns a list of the card ids or #f otherwise.
        ((= cmd *strat-cmd-flip-two*)
            (strat-level2-flip-two player))
        
        (else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

; Returns (card1 card2)
(define (strat-level2-flip-two player)
    ; 1) On the first-round two-flip, open any two cards in separate columns.
    (define col1 (random-integer 4))
    (define col2 (random-integer-exclude 4 col1))
    (list 
        (+ (* col1 3) (random-integer 3))
        (+ (* col2 3) (random-integer 3)))
)

; Returns #t if a play was executed #f otherwise
(define (strat-level2-phase2 player)
    (BASE-PLAY player)
)

; Returns #t if a play was executed #f otherwise
(define (strat-level2-phase1 player)
    (cond
        ;    ---- If you have more than one hidden card ----
        ((> (player-api-num-cards-hidden player) 1)            
            (BASE-PLAY player))

        ;    ---- If you have only one hidden card ----
        ((= (player-api-num-cards-hidden player) 1)            
            (END-PLAY player))

        (else #f)
    )                 
)

; Returns #t if a play was executed #f otherwise
(define (BASE-PLAY player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define complete-idx (player-api-complete-column-card-idx player discard-value))
    (define hidden-card (player-api-random-hidden-card-id player))
    (cond

        ; 2) Using the discard complete a matching column if possible.
        (complete-idx
            (player-api-replace-card-from-discard! player complete-idx) 
            #t)

        ; 3) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
        ((and highest-open-card-val (< discard-value highest-open-card-val) (>= highest-open-card-val 5))
            (player-api-replace-card-from-discard! player highest-open-card-idx) 
            #t)

        ; 4) If the discard is 5 or lower, then replace any hidden card.
        ((and (<= discard-value 5) highest-open-card-val (< highest-open-card-val 5))
            (player-api-replace-card-from-discard! player highest-open-card-idx)
            #t)

        ; 5) Otherwise; draw a card.
        (else
            (let ((draw-value (player-api-draw-card player)))
                (let ((complete-idx (player-api-complete-column-card-idx player draw-value)))
                    (cond
                        ; 6) Using the drawn card complete a matching column if possible
                        (complete-idx
                            (player-api-replace-card-with-draw-card! player complete-idx draw-value)
                            #t)

                        ; 7) If the draw card is lower than the highest open card and the highest open card is 5 or greater then replace it.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val) (>= highest-open-card-val 5) )
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 8) If the draw card is 5 or lower, then replace any hidden card.
                        ((and hidden-card (<= draw-value 5))
                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; 9) Otherwise; discard it.
                            (else
                                (player-api-discard-draw-card! player draw-value 
                                    (player-api-random-hidden-card-id player))
                                #t))))))                
)

; Returns #t if a play was executed #f otherwise
(define (END-PLAY player)

        ; 10) Estimate your hand value, by adding all the open cards plus 5 for the hidden card.
        (define my-hand-value-estimate (player-hand-value-estimate player))
        ; 11) Estimate your opponents hand values, by adding up their open cards plus 5 points for each hidden card.
        (define lowest-opponent-value-estimate (player-lowest-opponent-value-estimate player))


        (cond
            ;    ---- If you your hand is low relative to the other players ----
            ((< my-hand-value-estimate lowest-opponent-value-estimate)
                (strat-level2-low-relative-to-other-players player))

            ;    ---- Otherwise do not replace the hidden card ----
            (else
                (strat-level2-do-not-replace-the-hidden-card player)))
)

(define (strat-level2-low-relative-to-other-players player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define hidden-card (player-api-random-hidden-card-id player))

    (cond
        ; 12) Using the discard complete a matching column if possible.
        ((player-api-complete-column-card-idx player discard-value)
            (player-api-replace-card-from-discard! player (player-api-complete-column-card-idx player discard-value)) 
            #t)

        ; 13) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
        ((and highest-open-card-val (< discard-value highest-open-card-val) (>= highest-open-card-val 5))
            (player-api-replace-card-from-discard! player highest-open-card-idx) 
            #t)

        ; 14) If the discard is 5 or lower, then replace any hidden card.
        ((and (<= discard-value 5) highest-open-card-val (< highest-open-card-val 5))
            (player-api-replace-card-from-discard! player highest-open-card-idx)
            #t)

        ; 15) Otherwise; draw a card.
        (else
            (let ((draw-value (player-api-draw-card player)))
                (let ((complete-idx (player-api-complete-column-card-idx player draw-value)))
                    (cond
                        ; 16) Using the drawn card complete a matching column if possible
                        (complete-idx
                            (player-api-replace-card-with-draw-card! player complete-idx draw-value)
                            #t)

                        ; 17) If the drawn card is 5 or lower replace the highest open card greater than 5.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val) )
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 18) If the drawn card is 5 or lower replace the hidden card.
                        ((and hidden-card (<= draw-value 5))
                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; 19) If it is lower than the highest open card replace it.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val))
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 20) Otherwise discard it.
                        (else
                            (player-api-discard-draw-card! player draw-value 
                                (player-api-random-hidden-card-id player))
                            #t))))))            

)

(define (strat-level2-do-not-replace-the-hidden-card player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define hidden-card (player-api-random-hidden-card-id player))

    (cond
        ; 21) If the discard is 5 or lower, replace: the highest open card.
        ((and highest-open-card-val (< discard-value highest-open-card-val))
            (player-api-replace-card-from-discard! player highest-open-card-idx)                
            #t)

        ; 22) Draw a card and replace the highest open card.
        (else 
            (let ((draw-value (player-api-draw-card player)))
                (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                #t))
                
            )
)


