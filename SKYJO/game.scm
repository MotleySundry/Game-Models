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

        ; Updated at the end of the game
        0                               ;winning-points
        #f                              ;winner-ties?
        "winner TBD"                    ;winner                          
        "winner-strat TBD"              ;winner-strat                         
    )
)

; Runs a game and returns #t if all was well.
(define (game-run game)
    (let loop ((i 0))
        (if (game-over? game)
            #t
            (let ((round (new-round i game)))
                (if (>= i *max-rounds*)(log-fatal "The game run has reached *max-rounds*" *max-rounds*))

                ; Add round to game
                (game-set-round! game i round)
                (let ((high-flipper (round-deal-hands round)))
                    ; Set the first player
                    (if (= i 0) (round-first-player-set! round high-flipper)
                            (round-first-player-set! round (remainder (+ *num-players* (round-first-player (game-get-round game (- i 1)))) *num-players*))))

                (let ((out-player (round-run round))) 
                        (game-calc-player-points game round out-player)
                        (loop (+ i 1)))
                #f)
            ))
)   

; Tallys the player scores for this round.
; Returns the highest player game points or #f if the game is over.
(define (game-tally-player-points game round)
    #f
)

; GAME METHODS

(define (game-add-player-score! game id round-score)
    (game-set-player-score! game id (+ round-score (game-player-score game id)))
)

(define (game-set-player-score! game id score)
    (vector-set! (game-scores game) id score)
)

(define (game-player-score game id)
    (vector-ref (game-scores game) id)
)

(define (game-get-round game id)
    (vector-ref (game-rounds game) id)
)

(define (game-set-round! game id round)
    (vector-set! (game-rounds game) id round)
)

