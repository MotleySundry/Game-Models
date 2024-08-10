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
    removed             ;total columns removed
    last-round          ;reference to the round that ended the game
    starter             ;integer the starter for the round

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
        "last-round TBD"                ;last-round
        "starter TBD"                   ;starter

        ; Updated at the end of the game
        0                               ;winning-points
        #f                              ;winner-ties?
        "winner TBD"                    ;winner                          
        "winner-strat TBD"              ;winner-strat                         
    )
)

(define (game-run game)
    (let loop ((i 0))
        (if (>= i *max-rounds*)
            (log-fatal "The game run has reached *max-rounds*" *max-rounds*)

            ; RUN NEXT ROUND
            (let ((round (new-round i game)))
                (game-set-round! game i round)
                (game-last-round-set! game round)
                (game-num-rounds-set! game (+ i 1))
                (round-is-valid? round)

                (let ((high-flip (round-deal-hands round)))
                    ; set starting player
                    (if (= i 0)
                        (game-starter-set! game high-flip)
                        (game-starter-set! game (remainder ( + (game-starter game) *num-players*) *num-players*)))
                    (round-set-first-player! round (game-starter game))

                    ; run it
                    (round-run round)
                    (round-is-valid? round)

                    (game-tally-player game round)

                    ; game done
                    (if (< (vector-max-val (game-points game)) 100)
                        (loop (+ i 1))
                        )))))
)

; Tallys the player scores for this round.
(define (game-tally-player game round)
    (let loop ((i 0))
        (if (< i *num-players*)
            (let ((player (round-get-player round i)))
                (game-add-player-points game i (hand-card-sum (player-hand player)))
                (game-add-player-removed game i (player-removed player))
                (loop (+ i 1)))))
)

; GAME GETTERS

(define (game-get-player-points game id)
    (vector-ref (game-points game) id)
)

(define (game-get-player-removed game id)
    (vector-ref (game-removed game) id)
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

; REMOVED CNT
(define (game-set-player-removed! game id removed)
    (vector-set! (game-removed game) id removed)
)

(define (game-add-player-removed game id removed)
    (game-set-player-removed! game id (+ removed (game-get-player-removed game id)))
)


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

