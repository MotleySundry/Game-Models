; Motley Sundry :: Game Models :: library.scm
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

; Return  the median of a list of integers or #f if the list is empty.
(define  (list-get-median list)
    #f
)

; Duplicate a vector.
(define (vector-dup vect)
    (define (myfun vect new i)
        (if (>= i 0) (vector-set! new i (vector-ref vect i) ))
        (if (> i 0) (myfun vect new (- i 1))) )
    (let 
        ((new (make-vector(vector-length vect))))
            (myfun vect new (- (vector-length vect) 1))
            new)
)

; Divides a vector by a scalar and puts the result into a second.
; The to can be the from vector.
(define (vector-divide-scalar from to scalar)
    (if (not (= (vector-length from) (vector-length to)))
        (log-fatal "The two vectors are not the same length"))

    (let loop ((i 0))
        (if (< i (vector-length from))
            (begin
                (vector-set! to i (/ (vector-ref from i) (+ 0.0 scalar)))
                (loop (+ i 1)))
        ))
)

; Adds two vectors and puts the result into a third.
; The to can be one of the from vectors.
(define (vector-add from1 from2 to)
    (if (not (= (vector-length from1) (vector-length from2) (vector-length to)))
        (log-fatal "The three vectors are not the same length"))

    (let loop ((i 0))
        (if (< i (vector-length from1))
            (begin
                (vector-set! to i (+ (vector-ref from1 i) (vector-ref from2 i)))
                (loop (+ i 1)))
        ))
)

; Copy a vector to a list.
(define (vector-to-list vect)
    (define (myfun vect lst i)
        (if (< i 0) 
            lst
            (myfun vect (cons (vector-ref vect i) lst) (- i 1))))
    (myfun vect '() (- (vector-length vect) 1))
)

; Randomize an vector in-place.
(define (vector-rand vect)
    (define (myfun vect len i)
        (let(
            (tmp (vector-ref vect i))
            (irnd (random-integer len)))
                (vector-set! vect i (vector-ref vect irnd) )
                (vector-set! vect irnd tmp)
                (if (> i 0) (myfun vect len (- i 1))) ))
    (myfun vect (vector-length vect) (- (vector-length vect) 1))
    vect
)

(define (random-integer-exclude max exclude)
    (define (myfun)
        (let ((rnd (random-integer max)))
        (if (= rnd exclude)
            (myfun)
            rnd))
    )
    (myfun)
)

(define (vector-max-val vector)
    (let loop ((i 0) (max-val (vector-ref vector 0)))
        (if (< i (vector-length vector))
            (if (> (vector-ref vector i) max-val)
                (loop (+ i 1) (vector-ref vector i))
                (loop (+ i 1) max-val))
        max-val))
)

(define (vector-max-val-idx vector)
    (let loop ((i 0) (max-idx 0) (max-val (vector-ref vector 0)))
        (if (< i (vector-length vector))
            (if (> (vector-ref vector i) max-val)
                (loop (+ i 1) i (vector-ref vector i))
                (loop (+ i 1) max-idx max-val))
        max-idx))
)

(define (vector-min-val vector)
    (let loop ((i 0) (max-val (vector-ref vector 0)))
        (if (< i (vector-length vector))
            (if (< (vector-ref vector i) max-val)
                (loop (+ i 1) (vector-ref i))
                (loop (+ i 1) max-val))
        max-val))
)

(define (vector-min-val-idx vector)
    (let loop ((i 0) (max-idx 0) (max-val (vector-ref vector 0)))
        (if (< i (vector-length vector))
            (if (< (vector-ref vector i) max-val)
                (loop (+ i 1) i (vector-ref i))
                (loop (+ i 1) max-idx max-val))
        max-idx))
)

; Displays everything on one line, a list is recursed
(define (print value)
    (let loop ((val value))
        (if (not (null? val))
                (if (list? val)
                    (begin (loop (car val)) (loop (cdr val)))
                    (begin (display val)(display " ")))))
    (newline)
)

; Displays values separate lines, a list is recursed
(define (println value)
    (let loop ((val value))
        (if (not (null? val))
                (if (list? val)
                    (begin (loop (car val)) (loop (cdr val)))
                    (begin (display val)(newline)))))
)

(define (log-fatal msg data)
    (display "### ")(display msg)(display " ### ")(display (time->seconds(current-time)))(newline)
    (println data)
    (exit 1)
)

(define (log-error msg data)
    (display "!!! ")(display msg)(display " !!! ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)

(define (log-warning msg data)
    (display "=== ")(display msg)(display " === ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)

(define (log-info msg data)
    (display "--- ")(display msg)(display " --- ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)



