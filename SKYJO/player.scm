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

(define-structure player
    id
    score 
    cards ;99 - not present
    card-up ;0 - face down 1 - face up
    strat ;lambda
)

; Create a new initialized player structure.
(define (new-player id)
    ; Allocate structure
    (make-player
        id ;id
        0  ;score
        (make-s8vector 12 99) ;cards
        (make-s8vector 12 0) ;card-up
        (get-player-strat id) ;strategy
    ))  

; Returns #f if the player terminates by turning up their last card.
(define (run-player player game sim-stats cmd)
    ((player-strat player) player game sim-stats cmd)
)

; Returns #t if any cards are face down.
(define (player-any-cards-down? player)
    (define (myfun i)
        (if (= i 12)
            #f
            (if (= (s8vector-ref (player-card-up player) i) 0)
                #t 
                (myfun (+ i 1))
            ))
    )
    (myfun 0)
)

; Returns the index of the largest up card or #f if there are no up cards.
 (define (player-largest-up-card-idx player)
     (define (myfun i max-val max-idx)
        (if (= i 12)
            max-idx
            (if (and
                    (= (s8vector-ref (player-card-up player) i) 1)
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


