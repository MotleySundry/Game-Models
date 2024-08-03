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

(define-structure round
    id              ;integer round id, unique in context
    game            ;reference to the containing game
    first           ;integer id of the player who starts the round
    players         ;vector of player references, new for each round

    deck            ;reference to the deck, new for each round

)    

(define (new-round id game first)

    (let ((round 
            (make-round
                id ; round id integer
                game ; parent game pointer
                (make-vector *num-players*) ; players vector of pointers
                (new-deck) ; deck pointer
                first-player ; first player id integer
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

; Returns the id of the first player to open their last card. 
(define (round-run round)
   (define out-player (run-phase1 round))
   (round-update-player-totals round)
   (round-update-player-points round out-player)
)

 (define (round-update-player-totals round)
    #t
)

; Calculates the player scores for the round.
(define (round-update-player-points game round out-player)
    (let ((min-id (round-min-score round exclude-player)))
        (let loop ((i 0))
            (cond
                ((< i *num-players*)
                    (cond
                        ((= i exclude-player)
                            (if (< (game-player-score game i) (game-player-score game min-id))
                                (game-add-player-score! game 1 (game-player-score game i))                            
                                (game-add-player-score! game 1 ( * 2 (game-player-score game i)))))

                        (else(game-add-player-score! game 1 (game-player-score game i)))))              
                (else (loop (+ i 1))))))
)


; Returns the player with the lowest card total for the round.
; Excludes the player that went out because a tie has to be detected for scoring.
(define (round-min-total round exclude-player)
    (let loop ((i 0)( min-id #f) (min-val 144))
        (if (= i *num-players*)
            min-id
            (if (and (not (= i exclude-player))(< (round-player-score round i) min-val))
                (loop (+ i 1) (round-player-score round i) i)
                (loop (+ i 1) min-id min-val))))
)

; Returns the id of the first player to open their last card. 
(define (run-phase1 round)
    (let loop ((i 0) (player-id (round-first-player round)))
        (if (>= i *round-max-plays*)
            (log-fatal "Phase1 has reached *round-max-plays*" i)
            (let ((player (round-get-player round player-id))) 
                (if (player-play-phase1 player)
                    (loop (+ i 1) (remainder (+ player-id 1)  *num-players*))
                    (run-phase2 round (remainder (+ player-id 1)  *num-players*))))
        )
        player-id
    )
)

(define (run-phase2 round start-player)
    (let loop ((i 0) (player-id start-player))
        (if (< i (- *num-players* 1))
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

(define (round-player-score round id)
    (player-score (round-get-player round id))
)

(define (round-get-player round id)
    (vector-ref (round-players round) id)
)

(define (round-set-player! round id player)
    (vector-set! (round-players round) id player)
)



