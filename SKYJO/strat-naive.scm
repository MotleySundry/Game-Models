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

    (define high-open-id (player-get-max-open-card player))
    (define discard-value (deck-discard-value (player-get-deck player)))
    (define hidden-id (player-first-hidden-card player))

        (cond
            ; Try replacing the highest open card with the discard top
            ((and high-open-id (< top-card (player-get-card player high-open-id)))
                (deck-pop-discard-pile! (player-get-deck player))
                (deck-push-discard-pile! (player-get-deck player) (player-get-card player high-open-id))
                (player-set-card! player high-open-id discard-value)
                #t)
                    
            ; Try replacing the hidden card with the discard top
            ((and hidden-id (<= discard-value *deck-median*))
                (deck-push-discard-pile! (player-get-deck player) (player-get-card player hidden-id))
                (player-set-card! player hidden-id discard-value)
                (player-open-card! player hidden-id)
                #t)
                        
            (else
                    ; Draw a card
                    (let ((draw (deck-pop-draw-pile! (player-get-deck player))))
                    (cond
                        ; Try replacing the highest open card with the draw
                        ((and high-open-id (< draw (player-get-card player high-open-id)))
                            (deck-push-discard-pile! (player-get-deck player) (player-get-card player high-open-id))
                            (player-set-card! player high-open-id draw)
                            #t)

                        ; Try replacing the hidden card with the draw
                        ((and hidden-id (<= discard-value *deck-median*))
                            (deck-push-discard-pile! (player-get-deck player) (player-get-card player hidden-id))
                            (player-set-card! player hidden-id draw)
                            (player-open-card! player hidden-id)
                            #t)

                        ; Discard the draw
                        (else
                        (deck-push-discard-pile! (player-get-deck player) (player-get-card player hidden-id)))))))                 
)

(define (strat-naive-flip-two player)
    
    (define card1 (random-integer *player-num-cards*))
    (define card2 (random-integer-exclude *player-num-cards* card1))

    (player-open-card! player card1)
    (player-open-card! player card2)

    (+ (player-get-card player card1) (player-get-card player card2))
)