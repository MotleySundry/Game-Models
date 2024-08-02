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

(define-structure simulation id games)

(define (new-simulation id)
    (make-simulation
        id
        (make-vector  *num-games*) ; games
    )
)

(define (simulation-run simulation)    
    
    (let loop ((i 0) )
        (let ((game (new-game i))) 
            (simulation-set-game! simulation i game) ; Add game to simulation

        (game-run game)

        (if (< (+ i 1) *num-games*)
            (loop (+ i 1))))
    )  
)

; SIMULATION ACCESSORS

(define (simulation-get-game simulation id)
    (vector-ref (simulation-games simulation) id)
)

(define (simulation-set-game! simulation id game)
    (vector-set! (simulation-games simulation) id game)
)


