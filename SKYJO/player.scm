; Motley Sundry :: Game Models :: SKYJO :: player.scm
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

(define *card-state-hidden* 0)
(define *card-state-open* 1)
(define *card-state-removed -1)

(define-structure player
    id
    score 
    cards ;value -2 to 12
    card-state ;0=hidden,  1=open, -1=removed
    strat ;lambda
)

; Create a new initialized player structure.
(define (new-player id)
    ; Allocate structure
    (make-player
        id ;id
        0  ;score
        (make-s8vector 12 0) ;cards (val 0)
        (make-s8vector 12 0) ;card-state (face down)
        (get-player-strat id) ;strategy
    ))  

; Returns #f if the player terminates by turning up their last card.
(define (run-player player game sim-stats cmd)
    (display " p")(display (player-id player))(display " ")
    ((player-strat player) player game sim-stats cmd)
)

; Returns the count if the cards in state or #f for failure.
(define (player-count-cards-in-state? player state)
    (define (myfun i cnt)
        (if (= i 12)
            cnt
            (if (= (s8vector-ref (player-card-state player) i) state)
                (myfun (+ i 1) (+ cnt 1))
                (myfun (+ i 1) cnt)
            ))
    )
    (myfun 0 0)
)

; Returns the index of the largest open card or #f if there are no open cards.
 (define (player-largest-open-idx player)
     (define (myfun i max-val max-idx)
        (if (= i 12)
            max-idx
            (if (and
                    (= (s8vector-ref (player-card-state player) i) *card-state-open*)
                    (or
                        (not max-idx)
                        (> (s8vector-ref (player-cards player) i) max-val)
                    ))
                (myfun (+ i 1) (s8vector-ref (player-cards player) i) i)
                (myfun (+ i 1) max-val max-idx)
            ))
    )
    (myfun 0 -2 #f)
 )

; Returns the value of the card replaced of #f on failure.
(define (player-replace-rand-hidden-card player val)
    (let ((idx (random-integer 12)))
        (if (= (s8vector-ref(player-card-state) idx) *card-state-hidden)
            (player-replace-card player idx val)
            (player-replace-rand-hidden-card player val)))
)

; Replaces the value and opens the card state.
(define (player-replace-card player idx val)
    (s8vector-set!(player-cards player) idx val)
    (player-set-card-state player idx *card-state-open*)
)

; Sets the card state
(define (player-set-card-state player idx state)
    (s8vector-set! (player-card-state player) idx state)
)

; Set sets the state of a randon hidden card to open 
(define (player-open-rand-hidden-card player)
    (let ((idx (random-integer 12)))
    (if (= (s8vector-ref(player-card-state player) idx) *card-state-hidden*)
        (player-set-card-state player idx *card-state-open*)
        (player-open-rand-hidden-card player)))
)




