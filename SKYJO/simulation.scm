; Motley Sundry :: Game Models :: SKYJO :: simulation.scm
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

(define-structure simulation id num-games game-scores)

(define (new-simulation id num-games)
    (make-simulation
        id
        num-games
        (new-vector2  num-games *num-players*)          ; game-scores
    )
)

(define (simulation-run sim)    
    
    (let loop ((i 0) )
        (let ((game (new-game i))) 
            ;(simulation-set-game! simulation i game) ; Add game to simulation

        (game-run game)
        (vector2-row-set! (simulation-game-scores sim) i (game-points game))

        (if (< (+ i 1) (simulation-num-games sim))
            (loop (+ i 1))))
    )
    (simulation-print sim "")  
)


; SIMULATION ACCESSORS

(define (simulation-get-game simulation id)
    (vector-ref (simulation-games simulation) id)
)

(define (simulation-set-game! simulation id game)
    (vector-set! (simulation-games simulation) id game)
)

; SIMULATION PRINT
(define (simulation-print sim tab)
    (println tab "--- Simulation ---")
    (println tab "id:         " (simulation-id sim))
    (println tab "num-games:  " (simulation-num-games sim))
)



