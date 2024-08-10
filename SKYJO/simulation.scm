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

(define-structure simulation 
    id 
    num-games
    last-game 
    game-scores 
    player-strat
    player-mean 
    player-std
    player-median
    player-max 
    player-min
)

(define (new-simulation id num-games)
    (make-simulation
        id
        num-games
        "last-game"
        (new-vector2  num-games *num-players*)  ;game-scores
        (make-vector *num-players*) ;player-strat
        (make-vector *num-players*) ;player-mean
        (make-vector *num-players*) ;player-std
        (make-vector *num-players*) ;player-median
        (make-vector *num-players*) ;player-max
        (make-vector *num-players*) ;player-min
    )
)

(define (simulation-run sim)    
    (let loop ((i 0) )
        (if (< (+ i 1) (simulation-num-games sim))
            (begin
                (let ((game (new-game i))) 
                (game-run game)
                (simulation-last-game-set! sim game)
                (vector2-row-set! (simulation-game-scores sim) i (game-points game))
                (loop (+ i 1))))))
    
    (simulation-calc-stats sim)
    (simulation-print sim "")  
)

(define (simulation-calc-stats sim)
    (define (mapping v) (floor (+ 0.5 v)))
    (define game-scores (simulation-game-scores sim))
    (define players (round-players (game-last-round (simulation-last-game sim))))
    (let loop ((i 0))
        (if (< i *num-players*)
            (let ((player-scores (vector2-get-column game-scores i)))
                (vector-set! (simulation-player-mean sim) i
                    (vector-mean player-scores))
                (vector-set! (simulation-player-std sim) i
                    (vector-standard-deviation player-scores))
                (vector-set! (simulation-player-median sim) i
                    (vector-median player-scores))
                (vector-set! (simulation-player-max sim) i
                    (vector-max player-scores))
                (vector-set! (simulation-player-min sim) i
                    (vector-min player-scores))
                (vector-set! (simulation-player-strat sim) i
                    (player-get-strat-label (vector-ref players i)))
                (loop (+ i 1))
            )
    ))

    (simulation-player-mean-set! sim (vector-map (simulation-player-mean sim) mapping))
    (simulation-player-median-set! sim (vector-map (simulation-player-median sim) mapping))
    (simulation-player-std-set! sim (vector-map (simulation-player-std sim) mapping))
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
    (println tab "id:            " (simulation-id sim))
    (println tab "num-games:     " (simulation-num-games sim))
    (println tab "player-strat:  " (simulation-player-strat sim))
    (println tab "player-median: " (simulation-player-median sim))
    (println tab "player-mean:   " (vector->real(simulation-player-mean sim)))
    (println tab "player-std:    " (simulation-player-std sim))
    (println tab "player-max:    " (simulation-player-max sim))
    (println tab "player-min:    " (simulation-player-min sim))
)



