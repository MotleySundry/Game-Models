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

(define cards '#s8(
    -2 -2 -2 -2 -2
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12
    -1 1 2 3 4 5 6 7 8 9 10 11 12))

(define-structure game draw-pile discard-pile players first-player)

; Create a new initialized game structure.
(define (new-game num-players)
    ; Allocate structure
    (define game (make-game
        (s8vector-to-list (s8vector-rand (s8vector-dup cards)))  ;draw-pile
        '() ;discard-pile
        (make-vector num-players) ;players
        0 ;first player
    ))
    
    ; Populate players vector
    (define (set-player id)
        (if (< id num-players)
            (begin
                (vector-set! (game-players game) id (new-player id))
                (set-player (+ id 1))))
    )
    (set-player 0)
    game
)

; Removes the top card on the draw pile.
; Returns the value of the card or #f if the draw pile is empty.
(define (game-pop-draw-pile game)
    ; Draw pile empty?
    (if (null? (game-draw-pile game))
        (begin (display "--- The draw pile is empty.")(newline)
        #f)
    
        ; Pop it off
        (let ((card (car (game-draw-pile game))))
            (game-draw-pile-set! game (cdr (game-draw-pile game)))
            card
        ))
)

; Removes the top card on the discard pile.
; Returns the value of the card or #f if the discard pile is empty.
(define (game-pop-discard-pile game)
    ; Discard pile empty?
    (if (null? (game-discard-pile game))
        (begin (display "--- The discard pile is empty.")(newline)
        #f)
    
        ; Pop it off
        (let ((card (car (game-discard-pile game))))
            (game-discard-pile-set! game (cdr (game-discard-pile game)))
            card
        ))
)

; Returns the value of the top card on the discard pile.
; Returns #f if the discard pile is empty.
(define (game-discard-top game)
    (if (null? (game-discard-pile game))
        (begin (display "--- The discard pile is empty.")(newline) #f)
        (car (game-discard-pile game)))
)

; Places the value of the card onto the discard pile.
(define (game-push-discard-pile game card)
    (game-discard-pile-set! game (cons card (game-discard-pile game)))
    #t
)

; Turns up two cards for each player.
; Returns first player id.
(define (game-deal-hands game sim-stats num-players)
    (define players (game-players game))
    (define (deal-card num cnt)
        (if (< cnt num)
            (let(
                (player (vector-ref players (remainder cnt num-players))))
                (s8vector-set!(player-cards player) (floor (/ cnt num-players)) (game-pop-draw-pile game))
                (deal-card num (+ cnt 1))
            )))

    ; Deal all hands
    (deal-card (* 12 num-players) 0)
    
    ; Add the first card to the discard pile.
    (game-push-discard-pile game(game-pop-draw-pile game))

    ; Return first player.
    (game-flip-two game sim-stats num-players)

)

(define (game-flip-two game sim-stats num-players)
    (define players (game-players game))
    (define (flip-two cnt max max-player)
            (if (< cnt num-players)
                (let(
                    (total (run-player (vector-ref players cnt) game sim-stats "flip-two")))
                    (if (> total max)
                        (flip-two (+ cnt 1) total cnt)
                        (flip-two (+ cnt 1) max max-player))
                )
                max-player; Return the player with the highest total
            ))
    (newline)(display "flip-two:")
    (flip-two 0 -1 0)
)

(define (run-game game sim-stats num-players)
    (define starting-player (game-deal-hands game sim-stats num-players))
    ; Run rounds until one player turns up their last card
    (define (phase-1 game sim-stats num-players first rnd-cnt)
        (display " r")(display rnd-cnt)
        (if (run-phase-1-round game sim-stats num-players first)
            (phase-1 game sim-stats num-players (remainder (+ first num-players) num-players) (+ rnd-cnt 1))))
    (newline)(display "phase 1:")
    (phase-1 game sim-stats num-players starting-player 0)
)

; Returns #f if the game is over
(define (run-phase-1-round game sim-stats num-players first)
    ; Run round until finished or one player turns up their last card
    (define (run-plays cnt id)
            (if (= cnt num-players)
                #t 
                (if (run-player (game-get-player game id) game sim-stats "phase-1")
                    (run-plays  (+ cnt 1) (remainder (+ id 1) num-players))
                    (begin
                        (phase-2 game sim-stats num-players id)
                        #f))))
        
    (run-plays 0 first)
)

(define (phase-2 game sim-stats num-players skip)
    (newline)(display "phase 2:")
    (display " r1")
    (run-phase-2-round game sim-stats num-players skip)
    (display " r2")
    (run-phase-2-round game sim-stats num-players skip)
)
    
(define (run-phase-2-round game sim-stats num-players skip)
    (define (run-round num id)
        (if (= num (- num-players 1))
            #t
            (begin
                (run-player (game-get-player game id) game sim-stats "phase-2")
                (run-round  (+ num 1) (remainder (+ id 1) num-players)))))

    (run-round 0 (remainder (+ skip 1) num-players))
)

(define (game-get-player game id)
    (vector-ref (game-players game) id)
)

