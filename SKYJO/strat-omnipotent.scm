; Motley Sundry :: Game Models :: SKYJO :: strat-omnipotent.scm
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

; The most powerful strategy with the ability to break the rules by looking at all cards.
; It is useful to measure the random nature of the game and inform legal stragegy development.
(define (strat-omnipotent player cmd)

    (cond
        ; Returns the string label of the strategy or #f on failure.
        ((= cmd *strat-cmd-get-label*) "Omnipotent")

        ; Returns #t if the play was executed or #f otherwise
        ((= cmd *strat-cmd-play-phase1*)
            (or 
                (strat-omnipotent-any-phase player)
                (log-fatal "Player failed to make a play: play-phase1" player))
        )
        
        ; Returns #t if the play was executed or #f otherwise.
        ((= cmd *strat-cmd-play-phase2*)
            ;(print (list "Phase2" (player-id player)))
            (or 
                (strat-omnipotent-any-phase player)
                (log-fatal "Player failed to make a play: play-phase2" player)))

        ; Returns the sum of the cards if the flips were executed #f otherwise.
        ((= cmd *strat-cmd-flip-two*)
            (strat-omnipotent-flip-two player))
        
        (else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

; Returns #t if the a play was executed #f otherwise
(define (strat-omnipotent-any-phase player)

    (define high-open-card-idx      (player-api-get-highest-open-card player))
    (define high-hidden-card-idx    (player-cheat-get-highest-hidden-card player))
    (define discard-val (player-api-get-discard-val player))
    (define draw-val (player-cheat-next-draw-card-val player))

    (cond 
        ; Both Open and Hidden are available (Open is higher) 
        ((and high-open-card-idx high-hidden-card-idx 
            (>= (player-api-get-open-card-value player high-open-card-idx) 
            (player-cheat-get-card-value player high-hidden-card-idx ))
                (or
                    (strat-omnipotent-try-open 
                        draw-val discard-val 
                        high-open-card-idx)

                    (strat-omnipotent-try-hidden
                            draw-val discard-val 
                            high-hidden-card-idx)
                            
                    (player-api-discard-draw-card! player (player-api-draw-card player)
                        (player-api-random-hidden-card-id player))))
            #t)

        ; Both Open and Hidden are available (Hidden is higher 
        ((and high-open-card-idx high-hidden-card-idx 
            (<= (player-api-get-open-card-value player high-open-card-idx) 
                (player-cheat-get-card-value player high-hidden-card-idx ))

                (or
                    (strat-omnipotent-try-hidden
                            draw-val discard-val 
                            high-hidden-card-idx)

                    (strat-omnipotent-try-open 
                        draw-val discard-val 
                        high-open-card-idx)
                            
                    (player-api-discard-draw-card! player (player-api-draw-card player)
                        (player-api-random-hidden-card-id player))))
            #t)

        ; Only Open is available  
        (high-open-card-idx
            (or
                (strat-omnipotent-try-open 
                    draw-val discard-val 
                    high-open-card-idx)

                    (player-api-discard-draw-card! player (player-api-draw-card player)
                        (player-api-random-hidden-card-id player)))
            #t)

        ; Only Hidden is available  
        (high-hidden-card-idx
            (or
                (strat-omnipotent-try-hidden
                        draw-val discard-val 
                        high-hidden-card-idx)

                    (player-api-discard-draw-card! player (player-api-draw-card player)
                        (player-api-random-hidden-card-id player)))
            #t)

        ; Neither Open or Hidden is a vailable
        (else
            (player-api-discard-draw-card! player  (player-api-draw-card player)     
                (player-api-random-hidden-card-id player))
        #t)
    )
)

; Returns #t if the exchange was made, #f otherwise
(define (strat-omnipotent-try-open draw-val discard-val high-open-card-idx)
    (define high-open-card-val      (player-api-get-open-card-value player high-open-card-idx))

    (cond     
        ; Discard --> Open 
        ((and (<= discard-val draw-val) (< discard-val high-open-card-val))
            (player-api-replace-card-from-discard! player)
            #t)
        
        ; Draw --> Open 
        ((< draw-val high-open-card-val)
            (player-api-replace-card-with-draw-card! player high-open-card (player-api-draw-card player))
            #t)
            
        (else #f))
)

; Returns #t if the exchange was made, #f otherwise
(define (strat-omnipotent-try-hidden draw-val discard-val high-hidden-card-idx)
    (define high-hidden-card-val    (player-cheat-get-card-value player high-hidden-card-idx ))

    (cond     
        ; Discard --> hidden 
        ((and (<= discard-val draw-val) (< discard-val high-hidden-card-val))
            (player-api-replace-card-from-discard! player)
            #t)
        
        ; Draw --> hidden 
        ((< draw-val high-hidden-card-val)
            (player-api-replace-card-with-draw-card! player high-hidden-card (player-api-draw-card player))
            #t)
            
        (else #f))
)

; Returns (card1 card2)
(define (strat-omnipotent-flip-two player)
    (define card1 (random-integer *hand-num-cards*))
    (list 
        card1
        (random-integer-exclude *hand-num-cards* card1))
)