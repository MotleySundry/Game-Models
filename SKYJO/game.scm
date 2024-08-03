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

                (let ((high-flip (round-deal-hands round)))
                    ; set starting player
                    (if (= i 0)
                        (game-set-starter! game high-flip)
                        (game-set-starter! game (remainder ( + (game-get-starter game) *num-players*) *num-players*)))
                    (round-set-first-player! round (game-get-starter game))

                    ; run it
                    (round-run round)

                    ; game done
                    (if (game-tally-player-points game round)
                        (loop (+ i 1)))))))
)

; Tallys the player scores for this round.
; Returns #t or #f if the game is over.
(define (game-tally-player-points game round)
    #f
)

; GAME METHODS

(define (game-set-starter! game starter)
    (game-starter-set! game starter)
)

(define (game-get-starter game)
    (game-starter game)
)

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

