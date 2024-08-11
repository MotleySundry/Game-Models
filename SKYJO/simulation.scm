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
    num-rounds
    last-game 
    game-points 
    game-removed 
    game-penalties 
    player-strat
    player-removed
    player-penalties
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
        0                                       ;num-rounds
        "last-game"                             ;last-game
        (new-vector2  num-games *num-players*)  ;game-points
        (new-vector2  num-games *num-players*)  ;game-removed
        (new-vector2  num-games *num-players*)  ;game-penalties
        (make-vector *num-players*)             ;player-strat
        (make-vector *num-players*)             ;player-removed
        (make-vector *num-players*)             ;player-penalties
        (make-vector *num-players*)             ;player-mean
        (make-vector *num-players*)             ;player-std
        (make-vector *num-players*)             ;player-median
        (make-vector *num-players*)             ;player-max
        (make-vector *num-players*)             ;player-min
    )
)

(define (simulation-run sim)    
    (let loop ((i 0) )
        (if (< (+ i 1) (simulation-num-games sim))
            (begin
                (let ((game (new-game i))) 
                (game-run game)
                (simulation-last-game-set! sim game)
                (simulation-num-rounds-set! sim (+ (simulation-num-rounds sim) (game-num-rounds game)))
                (vector2-row-set! (simulation-game-points sim) i (game-points game))
                (vector2-row-set! (simulation-game-removed sim) i (game-removed game))
                (vector2-row-set! (simulation-game-penalties sim) i (game-penalties game))
                (loop (+ i 1))))))
    
    (simulation-calc-stats sim)
    (simulation-print sim "")  
)

(define (simulation-calc-stats sim)
    (define (mapping v) (floor (+ 0.5 v)))
    (define game-points (simulation-game-points sim))
    (define game-removed (simulation-game-removed sim))
    (define game-penalties (simulation-game-penalties sim))
    (define players (round-players (game-last-round (simulation-last-game sim))))
    (let loop ((i 0))
        (if (< i *num-players*)
            (let ((player-points (vector2-get-column game-points i))
                    (player-removed (vector2-get-column game-removed i))
                    (player-penalties (vector2-get-column game-penalties i)))

                ; STRATEGY
                (vector-set! (simulation-player-strat sim) i
                    (player-get-strat-label (vector-ref players i)))

                ; REMOVED
                (vector-set! (simulation-player-removed sim) i
                    (vector-sum player-removed))

                ; PENALTIES
                (vector-set! (simulation-player-penalties sim) i
                    (vector-sum player-penalties))

                ; PLAYER POINT STATISTICS
                (vector-set! (simulation-player-mean sim) i
                    (vector-mean player-points))
                (vector-set! (simulation-player-std sim) i
                    (vector-standard-deviation player-points))
                (vector-set! (simulation-player-median sim) i
                    (vector-median player-points))
                (vector-set! (simulation-player-max sim) i
                    (vector-max player-points))
                (vector-set! (simulation-player-min sim) i
                    (vector-min player-points))

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
    (println tab "Simulation Id:    " (simulation-id sim))
    (println tab "Num Games:        " (simulation-num-games sim))
    (println tab "Num Rounds:       " (simulation-num-rounds sim))
    (println tab "Player Strat:     " (simulation-player-strat sim))
    (println tab "Removed columns:  " (simulation-player-removed sim))
    (println tab "Penalty Points:   " (simulation-player-penalties sim))
    (println tab "Point Median:     " (simulation-player-median sim))
    (println tab "Point Mean:       " (vector->real(simulation-player-mean sim)))
    (println tab "Point STD:        " (simulation-player-std sim))
    (println tab "Point Max:        " (simulation-player-max sim))
    (println tab "Point Min:        " (simulation-player-min sim))
)



