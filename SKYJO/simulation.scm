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

(define-structure simulation id num-games games score-total score-mean)

(define (new-simulation id num-games)
    (make-simulation
        id
        num-games
        (make-vector  num-games "Games TBD")    ; games
        (make-vector  *num-players* 0)          ; score-total
        (make-vector  *num-players* 0.0)        ; score-mean
    )
)

(define (simulation-run simulation)    
    
    (let loop ((i 0) )
        (let ((game (new-game i))) 
            ;(simulation-set-game! simulation i game) ; Add game to simulation

        (game-run game)
        (simulation-tally-game simulation game)

        (if (< (+ i 1) (simulation-num-games simulation))
            (loop (+ i 1))))
    )
    (simulation-calc-means simulation)
    (simulation-print simulation "")  
)

(define (simulation-tally-game sim game)
    (vector-add (game-points game) (simulation-score-total sim) (simulation-score-total sim))
)

(define (simulation-calc-means sim)
    (vector-divide-scalar (simulation-score-total sim) (simulation-score-mean sim) 
        (simulation-num-games sim))
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
    (display tab)(print "--- Simulation ---")
    (display tab)(print (list "id:         " (simulation-id sim)))
    (display tab)(print (list "num-games:  " (simulation-num-games sim)))
    (display tab)(print (list "score-total:" (simulation-score-total sim)))
    (display tab)(print (list "score-mean: " (simulation-score-mean sim)))
    (newline)
)



