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

(define-structure game id rounds round-cnt )

(define (new-game id)
    (make-game
        id ; id
        (make-vector  *max-rounds*) ; rounds
        0 ; round-cnt
    )
)

(define (game-run game)
    
    (let loop ((i 0) (round (new-round i)))
        (if (>= i *max-rounds*)(begin (display "!!! The game run has reached *max-rounds*")(newline)(exit 1)))

        ; Add round to game
        (game-set-round! game i round)
        (let ((high-player (round-deal-hands round)))
            ; Set the round first player
            (if (= i 0) (round-first-player-set! round high-player)
                    (round-first-player-set! round (remainder (+ *num-players* ((round-first-player (game-get-round game (- i 1))))) *num-players*))))

        (loop (+ i 1)))
)

; GAME ACCESSORS

(define (game-get-round game id)
    (vector-ref (game-rounds game) id)
)

(define (game-set-round! game id round)
    (vector-set! (game-rounds game) id round)
)

