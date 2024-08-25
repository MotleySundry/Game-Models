; Motley Sundry :: Game Models :: SKYJO :: game.scm
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

(define-structure game 
    id

    ; Updated for each round
    rounds              ;vector of round references, filled as the game progresses
    num-rounds          ;integer incremented as new rounds are started
    points              ;integer points for each player tallied after each round
    removed             ;total columns removed for each player tallied after each round
    penalties           ;total penalties for each player tallied after each round
    plays               ;total penalties for each player tallied after each round
    last-round          ;reference to the round that ended the game

    ; Updated at the end of the game
    winning-points      ;integer points of the winner/s 
    winner-ties?        ;boolean were there ties for winner?
    winner              ;reference to the winning player, ties are broken randomly
    winner-strat        ;string label of the winner's strategy, ties are broken randomly 
)

(define (new-game id)
    (make-game
        id                              ;id
        
        ; Updated for each round
        (make-vector  *max-rounds*)     ;rounds 
        0                               ;round-count
        (make-vector  *num-players* 0)  ;points
        (make-vector  *num-players* 0)  ;removed
        (make-vector  *num-players* 0)  ;penalties
        (make-vector  *num-players* 0)  ;plays
        "last-round TBD"                ;last-round

        ; Updated at the end of the game
        0                               ;winning-points
        #f                              ;winner-ties?
        "winner TBD"                    ;winner                          
        "winner-strat TBD"              ;winner-strat                         
    )
)

(define (game-run game)
    (define last-out-player #f)
    (let loop ((i 0))
        (if (>= i *max-rounds*)
            (log-fatal "The game run has reached *max-rounds*" *max-rounds*)

            ; RUN NEXT ROUND
            (let ((round (new-round i game)))
                (game-set-round! game i round)
                (game-last-round-set! game round)
                (game-num-rounds-set! game (+ i 1))

                (let ((high-flip (round-deal-hands round (= i 0))))
                    ; set starting player
                    (if (= i 0)
                        (round-set-first-player! round high-flip)
                        (round-set-first-player! round last-out-player))

                    ; run it
                    (set! last-out-player (round-run round))

                    (game-tally-player game round)

                    ; game done
                    (if (< (vector-max-val (game-points game)) 100)
                        (loop (+ i 1))
                        )))))
)

; Tallys the player stats for this round.
(define (game-tally-player game round)
    (let loop ((i 0))
        (if (< i *num-players*)
            (let ((player (round-get-player round i)))
                (game-add-player-points game i (hand-total (player-hand player)))
                (game-add-player-removed game i (player-removed player))
                (game-add-player-penalties game i (player-penalties player))
                (game-add-player-plays game i (player-plays player))
                (loop (+ i 1)))))
)

; GAME GETTERS

(define (game-get-player-points game id)
    (vector-ref (game-points game) id)
)

(define (game-get-player-removed game id)
    (vector-ref (game-removed game) id)
)

(define (game-get-player-penalties game id)
    (vector-ref (game-penalties game) id)
)
(define (game-get-player-plays game id)
    (vector-ref (game-plays game) id)
)

(define (game-get-round game id)
    (vector-ref (game-rounds game) id)
)

; GAME SETTERS

; POINTS
(define (game-set-player-points! game id points)
    (vector-set! (game-points game) id points)
)

(define (game-add-player-points game id points)
    (game-set-player-points! game id (+ points (game-get-player-points game id)))
)

; PLAYS
(define (game-set-player-plays! game id plays)
    (vector-set! (game-plays game) id plays)
)

(define (game-add-player-plays game id plays)
    (game-set-player-plays! game id (+ plays (game-get-player-plays game id)))
)

; PENALTIES
(define (game-set-player-penalties! game id penalties)
    (vector-set! (game-penalties game) id penalties)
)

(define (game-add-player-penalties game id penalties)
    (game-set-player-penalties! game id (+ penalties (game-get-player-penalties game id)))
)

; REMOVED CNT
(define (game-set-player-removed! game id removed)
    (vector-set! (game-removed game) id removed)
)

(define (game-add-player-removed game id removed)
    (game-set-player-removed! game id (+ removed (game-get-player-removed game id)))
)

; ROUND
(define (game-set-round! game id round)
    (vector-set! (game-rounds game) id round)
)

; GAME PRINT
(define (game-print game tab)
    (println tab "--- Game ---")
    (println tab (list "id:       " (game-id game)))
    (println tab (list "num-rounds:" (game-num-rounds game)))
    (println tab (list "points:   " (game-points game)))
    (println tab (list "last-round:"))
    (round-print (game-last-round game) (string-append tab "  "))
    (newline)
)

