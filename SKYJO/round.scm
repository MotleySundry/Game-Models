; Motley Sundry :: Game Models :: SKYJO :: round.scm
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

(define-structure round id game players deck first-player)    

(define (new-round id game)

    (let ((round 
            (make-round
                id
                game
                (make-vector *num-players*) ; players
                (new-deck) ; deck
                0 ; first-player
            )))

        ; Create players
        (let loop ((i 0))
            (cond( 
                (< i *num-players*) 
                    (round-set-player! round i (new-player i round))
                    (loop (+ i 1)))))
        round
    )
)

(define (round-run round)
   (run-phase1 round)
)

(define (run-phase1 round)
    (let loop ((i 0) (player-id (round-first-player round)))
        (if (>= i *round-max-plays*)
            (log-fatal "Phase1 has reached *round-max-plays*" i)
            (let ((player (round-get-player round player-id))) 
                (if (player-play-phase1 player)
                    (loop (+ i 1) (remainder (+ player-id 1)  *num-players*))
                    (run-phase2 round (remainder (+ player-id 1)  *num-players*))))
        )
    )
)

(define (run-phase2 round start-player)
    (let loop ((i 0) (player-id start-player))
        (if (i < (- *num-players* 1))
            (let ((player (round-get-player round player-id)))
                (player-play-phase2 player)
                (loop (+ i 1) (remainder (+ player-id 1)  *num-players*))
            )
        )
    )
)

; Deals all player's hands.
; Turns up two cards for each player.
; Returns the player with the highest two cards, for the first player of round 0. 
(define (round-deal-hands round)
    ; Deal cards to each player
    (let loop ((i 0))
        (cond ( (< i *num-players*)
            (deal-player-hand (round-deck round) (round-get-player round i)) 
            (loop (+ i 1)))))

    ; Add the first card to the discard pile.
    (deck-push-discard-pile! (round-deck round) (deck-pop-draw-pile! (round-deck round)))

    ; each player flip two cards
    (round-flip-two round)
)

; Deals one player's hand.
(define (deal-player-hand deck player)
        ; For all player cards
        (let loop ((i 0))
            (cond ( (< i *player-num-cards*)
                (player-set-card! player i (deck-pop-draw-pile! deck)) 
                (loop (+ i 1)))))
)

; Turns up two cards for each player.
; Returns the player with the highest two cards, for the first player of round 0. 
(define (round-flip-two round)
    (let loop ((i 0) (max-val 0) (max-id 0))
        (if (< i *num-players*)
            (let ((player (round-get-player round i)))
                (let ((value (player-flip-two player)))
                    (if (> value max-val)
                        (loop (+ i 1) value i)
                        (loop (+ i 1) max-val max-id))))      
        max-id))
)

; ROUND ACCESSORS

(define (round-get-player round id)
    (vector-ref (round-players round) id)
)

(define (round-set-player! round id player)
    (vector-set! (round-players round) id player)
)



