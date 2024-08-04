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
    round-cnt           ;integer incremented as new rounds are started
    points              ;integer points for each player tallied after each round
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
                (game-round-cnt-set! game (+ i 1))
                (round-is-valid? round)

                (let ((high-flip (round-deal-hands round)))
                    ; set starting player
                    (if (= i 0)
                        (game-set-starter! game high-flip)
                        (game-set-starter! game (remainder ( + (game-get-starter game) *num-players*) *num-players*)))
                    (round-set-first-player! round (game-get-starter game))

                    ; run it
                    (round-run round)
                    (round-is-valid? round)

                    (game-tally-player-points game round)

                    ; game done
                    (if (< (vector-max-val (game-points game)) 100)
                        (loop (+ i 1))
                        ;(game-print game "")
                        (print (list "Game" ( + (game-id game) 1) "Points:" (game-points game)))
                        )))))
)

; Tallys the player scores for this round.
(define (game-tally-player-points game round)
    (let loop ((i 0))
        (if (< i *num-players*)
            (let ((player (round-get-player round i)))
                (player-get-card-sum player)
                (game-add-player-points game i (player-get-card-sum player))
                (loop (+ i 1)))))
)

; GAME GETTERS

(define (game-get-starter game)
    (game-starter game)
)

(define (game-get-player-points game id)
    (vector-ref (game-points game) id)
)

(define (game-get-round game id)
    (vector-ref (game-rounds game) id)
)

; GAME SETTERS

(define (game-set-player-points! game id points)
    (vector-set! (game-points game) id points)
)

(define (game-add-player-points game id points)
    (game-set-player-points! game id (+ points (game-get-player-points game id)))
)

(define (game-set-starter! game starter)
    (game-starter-set! game starter)
)

(define (game-set-player-score! game id score)
    (vector-set! (game-scores game) id score)
)

(define (game-set-round! game id round)
    (vector-set! (game-rounds game) id round)
)

; GAME PRINT
(define (game-print game tab)
    (display tab)(print "--- Game ---")
    (display tab)(print (list "id:       " (game-id game)))
    (display tab)(print (list "round-cnt:" (game-round-cnt game)))
    (display tab)(print (list "points:   " (game-points game)))
    (display tab)(print (list "last-round:"))
    (round-print (game-last-round game) (string-append tab "  "))
    (newline)
)

