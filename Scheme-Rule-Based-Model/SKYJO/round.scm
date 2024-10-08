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

(define (new-round id game)

    (let ((round 
            (make-round
                id                          ;id
                game                        ;game
                "first TBD"                 ;first
                (make-vector *num-players*) ;players
                (new-deck)                  ;deck
            )))

        ; Create players vector
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
   (round-update-all-player-points round out-player)
    out-player
)

; Calculates the player points for the round.
(define (round-update-all-player-points round out-player-id)
    (let loop ((i 0))
        (if (< i *num-players*)
            (begin
                (round-update-player-points round i out-player-id)
                (loop (+ i 1)))))
)

; Calculates a player's points for the round.
(define (round-update-player-points round player-id out-player-id)
    (define player (round-get-player round player-id))
    (define hand (player-hand player))
    (define hand-pts (hand-total hand))

    (if (not (= player-id out-player-id))
        ; Just set hand points if not out player
        (player-points-set! player hand-pts)
        
        ; Possible penalty?
        (if (>= hand-pts (round-min-hand round out-player-id))
            (if (< hand-pts 0)
                (player-points-set! player hand-pts) ; No penalty points with negative hand
                (begin ; Yes penalty points 
                    (player-points-set! player (* 2 hand-pts))
                    (player-penalties-set! player hand-pts)))))  
)

; Returns the player with the lowest card total for the round.
; Excludes the player that went out because a tie has to be detected for scoring.
(define (round-min-hand round exclude-player)
    (let loop ((i 0)( min-id #f) (min-val 144))
        (if (< i *num-players*)
            (if (and (not (= i exclude-player)) (< (hand-total (player-hand (round-get-player round i))) min-val))
                (loop (+ i 1) i (hand-total (player-hand (round-get-player round i))))
                (loop (+ i 1) min-id min-val))
            min-val))
)

; Returns the id of the first player to open their last card. 
(define (run-phase1 round)
    (let loop ((i 0) (player-id (round-first round)))
        (if (>= i *round-max-plays*)
            (log-fatal "Phase1 has reached *round-max-plays*" i)
            (let ((player (round-get-player round player-id))) 
                (if (player-play-phase1 player)
                    (loop (+ i 1) (remainder (+ player-id 1)  *num-players*))
                    (run-phase2 round (remainder (+ player-id 1)  *num-players*)))) ;Player opened last card
        )
        player-id)
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
; Returns the player with the highest two cards, or #f if flip-two? is false.
(define (round-deal-hands round flip-two?)
    ; Deal cards to each player
    (let loop ((i 0))
        (if (< i *num-players*)
            (begin
                (deal-hand (round-deck round) (player-hand (round-get-player round i)))
                (loop (+ i 1)))))

    ; Add the first card to the discard pile.
    (deck-push-discard-pile! (round-deck round) (deck-pop-draw-pile! (round-deck round)))

    ; each player flip two cards
    (if flip-two?
        (round-flip-two round)
        #f)
)

; Deals one player's hand.
(define (deal-hand deck hand)
        ; For all player cards
        (let loop ((i 0))
            (cond ( (< i *hand-num-cards*)
                (hand-set-card-value! hand i (deck-pop-draw-pile! deck)) 
                (loop (+ i 1)))))
)

; Turns up two cards for each player.
; Returns the player with the highest two cards, for the first player of round 0. 
(define (round-flip-two round)
    (let loop ((i 0) (max-val -4 ) (max-id 0))
        (if (< i *num-players*)
            (let ((player (round-get-player round i)))
                (let ((value (player-flip-two player)))
                    (if (> value max-val)
                        (loop (+ i 1) value i)
                        (loop (+ i 1) max-val max-id))))      
        max-id))
)

(define (round-get-player-points round id)
    (player-points (round-get-player round id))
)

(define (round-get-player round id)
    (vector-ref (round-players round) id)
)

(define (round-set-player! round id player)
    (vector-set! (round-players round) id player)
)

(define (round-set-first-player! round first)
    (round-first-set! round first)
)

; Validate the rounds's consistency
(define (round-is-valid? round)
    (let loop ((i 0))
        (if (< i *num-players*)
            (begin
                (player-is-valid? (round-get-player round 1))
                (loop (+ i 1)))))
)

; ROUND PRINT
(define (round-print round tab)
    (ptintln tab "--- Round ---")
    (ptintln tab (list "id:       " (round-id round)))
    (display tab)(deck-print (round-deck round) (string-append tab "  "))
    
    ;Players
    (let ploop ((i 0))
        (if (< i *num-players*)
            (begin
                (player-print (round-get-player round i) (string-append tab "  "))
                (ploop (+ i 1)))))

    (newline)
)


