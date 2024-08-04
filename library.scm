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
                (loop (+ i 1) (vector-ref i))
                (loop (+ i 1) max-val))
        max-val))
)

(define (vector-max-val-idx vector)
    (let loop ((i 0) (max-idx 0) (max-val (vector-ref vector 0)))
        (if (< i (vector-length vector))
            (if (> (vector-ref vector i) max-val)
                (loop (+ i 1) i (vector-ref i))
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


(define (displayln value)
    (display value)(newline)
)

(define (log-fatal msg data)
    (display "### ")(display msg)(display " ### ")(display (time->seconds(current-time)))(newline)
    (display "    ")(display data)(newline)
    (exit 1)
)

(define (log-error msg data)
    (display "!!! ")(display msg)(display " !!! ")(display (time->seconds(current-time)))(newline)
    (display "    ")(display data)(newline)
    data
)

(define (log-warning msg data)
    (display "=== ")(display msg)(display " === ")(display (time->seconds(current-time)))(newline)
    (display "    ")(display data)(newline)
    data
)

(define (log-info msg data)
    (display "--- ")(display msg)(display " --- ")(display (time->seconds(current-time)))(newline)
    (display "    ")(display data)(newline)
    data
)



