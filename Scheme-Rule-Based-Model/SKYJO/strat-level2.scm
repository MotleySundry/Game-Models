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
; =====================================
; === Steps for the Level2 strategy ===
; =====================================
; The second tier strategy that follows the rules and makes some round-level strategic decisions.
;
; #######################
; ###### TWO FLIP #######
; #######################

; 1) On the first-round two-flip, open any two cards in separate columns.
;
; ######################
; ###### PHASE 1 #######
; ######################

;    ---- BASE PLAY - If you have more than one hidden card ----
; 2) Using the discard complete a matching positive column if possible.
; 3) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 4) If the discard is 5 or lower, then replace any hidden card.
; 5) Otherwise; draw a card.
; 6) Using the drawn card complete a matching positive column if possible
; 7) If the draw card is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 8) If the draw card is 5 or lower, then replace any hidden card.
; 9) Otherwise; discard it.

;    ---- END PLAY - If you have only one hidden card ----
; 10) Estimate your hand value, by adding all the open cards plus 5 for the hidden card.
; 11) Estimate your opponents hand values, by adding up their open cards plus 5 points for each hidden card.
;
;    ---- TERMINATE ROUND - If you your hand is low relative to the other players ----
; 12) Using the discard complete a matching positive column if possible.
; 13) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
; 14) If the discard is 5 or lower, then replace any hidden card.
; 15) Otherwise, draw a card 
; 16) Using the drawn card complete a matching positive if possible 
; 17) If the drawn card is 5 or lower replace the highest open card greater than 5.
; 18) If the drawn card is 5 or lower replace the hidden card.
; 19) If the drawn card is lower than the highest open card replace it.
; 20) Otherwise discard it.

;    ---- PASS - Otherwise do not discard or replace the hidden card ----
; 21) If the discard is lower than the highest open card, replace it.
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
    (log-debug 1 "Level-2 Rule: 1" "player = " (player-id player))
    (list 
        (+ (* col1 3) (random-integer 3))
        (+ (* col2 3) (random-integer 3)))
)

; Returns #t if a play was executed #f otherwise
(define (strat-level2-phase2 player)
    (strat-level2-BASE-PLAY player)
)

; Returns #t if a play was executed #f otherwise
(define (strat-level2-phase1 player)
    (cond
        ;    ---- BASE PLAY - If you have more than one hidden card ----
        ((> (player-api-num-cards-hidden player) 1)            
            (strat-level2-BASE-PLAY player))

        ;    ---- END PLAY - If you have only one hidden card ----
        ((= (player-api-num-cards-hidden player) 1)            
            (strat-level2-END-PLAY player))

        (else #f)
    )                 
)

;    ---- BASE PLAY - If you have more than one hidden card ----
; Returns #t if a play was executed #f otherwise
(define (strat-level2-BASE-PLAY player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define complete-idx (player-api-complete-positive-column-card-idx player discard-value))
    (define hidden-card (player-api-random-hidden-card-id player))
    (cond

        ; 2) Using the discard complete a matching positive column if possible.
        (complete-idx
            (log-debug 1 "Level-2 Rule: 2" "player = " (player-id player) 
                "complete-id =" complete-idx
                "hand =" (player-hand player))

            (player-api-replace-card-from-discard! player complete-idx) 
            #t)

        ; 3) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
        ((and highest-open-card-val (< discard-value highest-open-card-val) (>= highest-open-card-val 5))
            (log-debug 1 "Level-2 Rule: 3" "player = " (player-id player)
                "highest-open-card-idx =" highest-open-card-idx
                "hand =" (player-hand player))

            (player-api-replace-card-from-discard! player highest-open-card-idx) 
            #t)

        ; 4) If the discard is 5 or lower, then replace any hidden card.
        ((and hidden-card (<= discard-value 5))
            (log-debug 1 "Level-2 Rule: 4" "player = " (player-id player)
                "hidden-card-idx =" hidden-card
                "discard-value =" discard-value
                "hand =" (player-hand player))

            (player-api-replace-card-from-discard! player hidden-card)
            #t)

        ; 5) Otherwise; draw a card.
        (else
            (let ((draw-value (player-api-draw-card player)))
                (log-debug 1 "Level-2 Rule: 5" "player = " (player-id player)
                    "draw-value =" draw-value)
            
                (let ((complete-idx (player-api-complete-positive-column-card-idx player draw-value)))
                    (cond
                        ; 6) Using the drawn card complete a matching positive column if possible
                        (complete-idx
                        (log-debug 1 "Level-2 Rule: 6" "player = " (player-id player) 
                            "complete-id =" complete-idx
                            "hand =" (player-hand player))

                            (player-api-replace-card-with-draw-card! player complete-idx draw-value)
                            #t)

                        ; 7) If the draw card is lower than the highest open card and the highest open card is 5 or greater then replace it.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val) (>= highest-open-card-val 5) )
                            (log-debug 1 "Level-2 Rule: 7" "player = " (player-id player)
                                "highest-open-card-idx =" highest-open-card-idx
                                "hand =" (player-hand player))

                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 8) If the draw card is 5 or lower, then replace any hidden card.
                        ((and hidden-card (<= draw-value 5))
                            (log-debug 1 "Level-2 Rule: 7" "player = " (player-id player)
                                "highest-open-card-idx =" highest-open-card-idx
                                "hand =" (player-hand player))

                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; 9) Otherwise; discard it.
                            (else
                                (log-debug 1 "Level-2 Rule: 9" "player = " (player-id player)
                                "highest-open-card-idx =" highest-open-card-idx
                                "hand =" (player-hand player))

                                (player-api-discard-draw-card! player draw-value 
                                    (player-api-random-hidden-card-id player))
                                #t))))))
)

;    ---- END PLAY - If you have only one hidden card ----
; Returns #t if a play was executed #f otherwise
(define (strat-level2-END-PLAY player)

        ; 10) Estimate your hand value, by adding all the open cards plus 5 for the hidden card.
        (define my-hand-value-estimate (player-api-my-hand-value-estimate player))
        ; 11) Estimate your opponents hand values, by adding up their open cards plus 5 points for each hidden card.
        (define lowest-opponent-value-estimate (player-api-lowest-opponent-value-estimate player))

        (log-debug 1 "Level-2 Rule: 10" "player = " (player-id player))
        (log-debug 1 "Level-2 Rule: 11" "player = " (player-id player))

        (cond
            ; Prevent infinite passing loop
            ((> (player-pass-cnt player) 5 )
                (log-debug 2 "Level-2 Prevented infinite passing loop" 
                    "player =" (player-id player) 
                    "hand =" (player-hand player))
                (strat-level2-TERMINATE-ROUND player))
            
            ;    ---- TERMINATE ROUND - If you your hand is low relative to the other players ----
            ((< my-hand-value-estimate (+ lowest-opponent-value-estimate (player-terminate-round-margin player)))
                (strat-level2-TERMINATE-ROUND player))

            ;    ---- PASS - Otherwise do not discard or replace the hidden card ----
            (else
                (player-pass-cnt-set! player (+ (player-pass-cnt player) 1))
                (strat-level2-PASS player)))
)

;    ---- TERMINATE ROUND - If you your hand is low relative to the other players ----
; Returns #t if a play was executed #f otherwise
(define (strat-level2-TERMINATE-ROUND player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define complete-idx (player-api-complete-positive-column-card-idx player discard-value))
    (define hidden-card (player-api-random-hidden-card-id player))

    (cond
        ; 12) Using the discard complete a matching positive column if possible.
        (complete-idx
            (log-debug 1 "Level-2 Rule: 12" "player = " (player-id player) complete-idx)
            (player-api-replace-card-from-discard! player complete-idx) 
            #t)

        ; 13) If the discard is lower than the highest open card and the highest open card is 5 or greater then replace it.
        ((and highest-open-card-val (< discard-value highest-open-card-val) (>= highest-open-card-val 5))
            (log-debug 1 "Level-2 Rule: 13" "player = " (player-id player))
            (player-api-replace-card-from-discard! player highest-open-card-idx) 
            #t)

        ; 14) If the discard is 5 or lower, then replace any hidden card.
        ((and hidden-card (<= discard-value 5))
            (log-debug 1 "Level-2 Rule: 14" "player = " (player-id player))
            (player-api-replace-card-from-discard! player hidden-card)
            #t)

        ; 15) Otherwise; draw a card.
        (else
            (let ((draw-value (player-api-draw-card player)))
                (let ((complete-idx (player-api-complete-positive-column-card-idx player draw-value)))
                    (cond
                        ; 16) Using the drawn card complete a matching positive column if possible
                        (complete-idx
                            (log-debug 1 "Level-2 Rule: 16" "player = " (player-id player))
                            (player-api-replace-card-with-draw-card! player complete-idx draw-value)
                            #t)

                        ; 17) If the drawn card is 5 or lower replace the highest open card greater than 5.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val) (>= highest-open-card-val 5) )
                            (log-debug 1 "Level-2 Rule: 17" "player = " (player-id player))
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 18) If the drawn card is 5 or lower replace the hidden card.
                        ((and hidden-card (<= draw-value 5))
                            (log-debug 1 "Level-2 Rule: 18" "player = " (player-id player))
                            (player-api-replace-card-with-draw-card! player hidden-card draw-value)
                            #t)

                        ; 19) If it is lower than the highest open card replace it.
                        ((and highest-open-card-idx (< draw-value highest-open-card-val))
                            (log-debug 1 "Level-2 Rule: 19" "player = " (player-id player))
                            (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                            #t)

                        ; 20) Otherwise discard it.
                        (else
                            (log-debug 1 "Level-2 Rule: 20" "player = " (player-id player))
                            (player-api-discard-draw-card! player draw-value 
                                (player-api-random-hidden-card-id player))
                            #t))))))            

)

;    ---- PASS - Otherwise do not discard or replace the hidden card ----
; Returns #t if a play was executed #f otherwise
(define (strat-level2-PASS player)
    (define highest-open-card-idx (player-api-get-highest-open-card-idx player))
    (define highest-open-card-val 
        (if highest-open-card-idx (player-api-get-open-card-value player highest-open-card-idx) #f))
    (define discard-value (player-api-get-discard-val player))
    (define hidden-card (player-api-random-hidden-card-id player))

    (cond
        ; 21) If the discard is lower than the highest open card, replace it.
        ((and highest-open-card-val (< discard-value highest-open-card-val))
            (log-debug 1 "Level-2 Rule: 21" "player = " (player-id player))
            (player-api-replace-card-from-discard! player highest-open-card-idx)                
            #t)

        ; 22) Draw a card and replace the highest open card.
        (else 
            (log-debug 1 "Level-2 Rule: 22" "player = " (player-id player))
            (let ((draw-value (player-api-draw-card player)))
                (player-api-replace-card-with-draw-card! player highest-open-card-idx draw-value)
                #t))
            )
)


