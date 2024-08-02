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

(define-structure game id rounds round-cnt scores)

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
                        (game-calc-player-scores round out-player)
                        (loop (+ i 1)))
                #f)
            ))
)

; Calculates the player scores for the round and adds it into the game score vector.
(define (game-calc-player-scores round out-player)
    (let ((min-id (game-min-score-exclude-player-id round out-player)))
        (let loop ((i 0))
            (cond
                ((< i *num-players*)
                    (cond
                        ((= i out-player)
                            (if (< (game-player-score game i) (game-player-score game min-id))
                                (game-add-player-score! game 1 (game-player-score game i))                            
                                (game-add-player-score! game 1 ( * 2 (game-player-score game i)))))

                        (else(game-add-player-score! game 1 (game-player-score game i)))))              
                (else (loop (+ i 1))))))
)

; Returns the player with the lowest raw score for the round, excludinf the player that went out.
(define (game-min-score-exclude-player-id round out-player)
    (let loop ((i 0)( min-id #f) (min-val 144))
        (if (= i *num-players*)
            min-id
            (if (and (not (= i out-player))(< (round-player-score round i) min-val))
                (loop (+ i 1) (round-player-scoregame i) i)
                (loop (+ i 1) min-id min-val))))
)

; Returns the id of the first player found to have a score of <= 100, #f otherwuse
(define (game-over? game)
    (let loop ((i 0))
        (if (= i *num-players*)
            #f
            (loop (+ i 1)))
    )
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

