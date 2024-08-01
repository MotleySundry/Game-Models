; Motley Sundry :: Game Models :: SKYJO :: table.scm
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

(define-structure table draw-pile discard-pile players first-player)

; Create a new initialized table structure.
(define (new-table num-players)
    ; Allocate structure
    (define table (make-table
        (s8vector-to-list (s8vector-rand (s8vector-dup cards)))  ;draw-pile
        '() ;discard-pile
        (make-vector num-players) ;players
        0 ;first player
    ))
    
    ; Populate players vector
    (define (set-player id)
        (if (< id num-players)
            (begin
                (vector-set! (table-players table) id (new-player id))
                (set-player (+ id 1))))
    )
    (set-player 0)
    table
)

; Removes the top card on the draw pile.
; Returns the value of the card or #f if the draw pile is empty.
(define (table-pop-draw-pile table)
    ; Draw pile empty?
    (if (null? (table-draw-pile table))
        (begin (display "--- The draw pile is empty.")(newline)
        #f)
    
        ; Pop it off
        (let ((card (car (table-draw-pile table))))
            (table-draw-pile-set! table (cdr (table-draw-pile table)))
            card
        ))
)

; Removes the top card on the discard pile.
; Returns the value of the card or #f if the discard pile is empty.
(define (table-pop-discard-pile table)
    ; Discard pile empty?
    (if (null? (table-discard-pile table))
        (begin (display "--- The discard pile is empty.")(newline)
        #f)
    
        ; Pop it off
        (let ((card (car (table-discard-pile table))))
            (table-discard-pile-set! table (cdr (table-discard-pile table)))
            card
        ))
)

; Returns the value of the top card on the discard pile.
; Returns #f if the discard pile is empty.
(define (table-discard-top table)
    (if (null? (table-discard-pile table))
        (begin (display "--- The discard pile is empty.")(newline) #f)
        (car (table-discard-pile table)))
)

; Places the value of the card onto the discard pile.
(define (table-push-discard-pile table card)
    (table-discard-pile-set! table (cons card (table-discard-pile table)))
    #t
)

; Turns up two cards for each player.
; Returns first player id.
(define (table-deal-hands table sim-stats num-players)
    (define players (table-players table))
    (define (deal-card num cnt)
        (if (< cnt num)
            (let(
                (player (vector-ref players (remainder cnt num-players))))
                (s8vector-set!(player-cards player) (floor (/ cnt num-players)) (table-pop-draw-pile table))
                (deal-card num (+ cnt 1))
            )))

    ; Deal all hands
    (deal-card (* 12 num-players) 0)
    
    ; Add the first card to the discard pile.
    (table-push-discard-pile table(table-pop-draw-pile table))

    ; Return first player.
    (table-flip-two table sim-stats num-players)

)

(define (table-flip-two table sim-stats num-players)
    (define players (table-players table))
    (define (flip-two cnt max max-player)
            (if (< cnt num-players)
                (let(
                    (total (run-player (vector-ref players cnt) table sim-stats "flip-two")))
                    (if (> total max)
                        (flip-two (+ cnt 1) total cnt)
                        (flip-two (+ cnt 1) max max-player))
                )
                max-player; Return the player with the highest total
            ))
    (newline)(display "flip-two:")
    (flip-two 0 -1 0)
)

(define (run-table table sim-stats num-players)
    (define starting-player (table-deal-hands table sim-stats num-players))
    ; Run rounds until one player turns up their last card
    (define (phase-1 table sim-stats num-players first rnd-cnt)
        (display " r")(display rnd-cnt)
        (if (run-phase-1-round table sim-stats num-players first)
            (phase-1 table sim-stats num-players (remainder (+ first num-players) num-players) (+ rnd-cnt 1))))
    (newline)(display "phase 1:")
    (phase-1 table sim-stats num-players starting-player 0)
)

; Returns #f if the table is over
(define (run-phase-1-round table sim-stats num-players first)
    ; Run round until finished or one player turns up their last card
    (define (run-plays cnt id)
            (if (= cnt num-players)
                #t 
                (if (run-player (table-get-player table id) table sim-stats "phase-1")
                    (run-plays  (+ cnt 1) (remainder (+ id 1) num-players))
                    (begin
                        (phase-2 table sim-stats num-players id)
                        #f))))
        
    (run-plays 0 first)
)

(define (phase-2 table sim-stats num-players skip)
    (newline)(display "phase 2:")
    (display " r1")
    (run-phase-2-round table sim-stats num-players skip)
    (display " r2")
    (run-phase-2-round table sim-stats num-players skip)
)
    
(define (run-phase-2-round table sim-stats num-players skip)
    (define (run-round num id)
        (if (= num (- num-players 1))
            #t
            (begin
                (run-player (table-get-player table id) table sim-stats "phase-2")
                (run-round  (+ num 1) (remainder (+ id 1) num-players)))))

    (run-round 0 (remainder (+ skip 1) num-players))
)

(define (table-get-player table id)
    (vector-ref (table-players table) id)
)

