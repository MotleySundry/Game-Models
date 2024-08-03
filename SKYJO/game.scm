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

(define-structure game id rounds round-cnt points last-round winner winner-points winner-strat)

(define (new-game id)
    (make-game
        id ; id
        (make-vector  *max-rounds*) ; rounds
        0 ; round-cnt
        (make-vector  *num-players* 0) ; scores
  
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
                (let ((high-player (round-deal-hands round)))
                    ; Set the round first player
                    (if (= i 0) (round-first-player-set! round high-player)
                            (round-first-player-set! round (remainder (+ *num-players* (round-first-player (game-get-round game (- i 1)))) *num-players*))))

                (let ((out-player (round-run round))) 
                        (game-calc-player-points game round out-player)
                        (loop (+ i 1)))
                #f)
            ))
)   

; Tallys the player the player scores for this round.
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

