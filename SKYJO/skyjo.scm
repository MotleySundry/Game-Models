; Motley Sundry - Game Models - SKYJO - skyjo.scm
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

(random-source-randomize! default-random-source)

(define *cards* '#s8(
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

(define-structure game
    deck discard players)

(define *game* (make-game 
    '() ; deck
    '() ; discard
    '() ; players
    ))

(define-structure player
    id score cards-value cards-up?)


; Duplicate an s8 vector.
(define (s8vector-dup vect)
    (define (myfun vect new i)
        (if (>= i 0) (s8vector-set! new i (s8vector-ref vect i) ))
        (if (> i 0) (myfun vect new (- i 1))) )
    (let 
        ((new (make-s8vector(s8vector-length vect))))
            (myfun vect new (- (s8vector-length vect) 1))
            new)
)

; Randomize an s8 vector in-place.
(define (s8vector-rand vect)
    (define (myfun vect len i)
        (let(
            (tmp (s8vector-ref vect i))
            (irnd (random-integer len))            )
                (s8vector-set! vect i (s8vector-ref vect irnd) )
                (s8vector-set! vect irnd tmp)
                (if (> i 0) (myfun vect len (- i 1))) ))
    (myfun vect (s8vector-length vect) (- (s8vector-length vect) 1))
    vect
)

(display (s8vector-dup *cards* ))
(newline)
(display (s8vector-rand (s8vector-dup *cards* )))






