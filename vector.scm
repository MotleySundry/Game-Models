; Motley Sundry :: Game Models :: vector.scm
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

(define (vector-count-values vect val)
    (let loop ((i 0) (cnt 0))
        (if (< i (vector-length vect))
            (if (= (vector-ref vect i) val)
                (loop (+ i 1) (+ cnt 1))
                (loop (+ i 1) cnt ))
            cnt))
)

; Returns the idx of the value or #f if it can't be found.
; Optionally an occurence of the value, where 0 is the first occurence.
(define (vector-get-value-idx vect val #!optional occurence)
    (define oc (if occurence occurence 0))
    (let loop ((i 0) (cnt -1) (idx #f))
        (if (and (< i (vector-length vect)) (not(= cnt oc)))
            (if (= (vector-ref vect i) val) 
                (loop (+ i 1) (+ cnt 1) i)
                (loop (+ i 1) cnt idx ))
 
        (if (= cnt oc) idx #f)))
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

; Returns a new vector with real elements.
(define (vector->real vect)
    (define new-vect (make-vector (vector-length vect)))
    (let loop ((i 0))
        (if (< i (vector-length vect))
            (begin
                (vector-set! new-vect i (+ 0.0 (vector-ref vect i)))
                (loop (+ i 1)))))
    new-vect
)

; Returns the median of the vector or #f if it doesn't exist.
(define (vector-median vect)
    (let
        ((tmp (vector-dup vect))
        (len (vector-length vect))
        (floor (floor/ len 2)))

        (vector-sort! (lambda (a b) (< a b)) vect)
        (vector-sort-in-place tmp)
        (cond 
            ((= len 0) #f)
            ((= len 1) (vector-ref vect 0))
            ((even? len) 
                (mean
                    (vector-ref vect floor)
                    (vector-ref vect (- floor 1))))
            (else (vector-ref vect floor))
        )
    )
)

(define (vector-mean vect)
    (/ (vector-sum vect) (vector-length vect))
)

(define (vector-standard-deviation vect) 
    (sqrt (vector-variance vect))
)

(define (vector-variance vect) 
    (define mean (vector-mean vect))
    (let loop ((i 0) (sum 0))
        (if (< i (vector-length vect))
            (loop (+ i 1) (+ sum (* (- (vector-ref vect i) mean) (- (vector-ref vect i) mean))))
            (/ sum (vector-length vect))))
)

(define (vector-max vect) 
    (let loop ((i 0) (max (vector-ref vect 0)))
        (if (< i (vector-length vect))
            (if (< max (vector-ref vect i))
                (loop (+ i 1) (vector-ref vect i))
                (loop (+ i 1) max))
            max))
)

(define (vector-min vect) 
    (let loop ((i 0) (min (vector-ref vect 0)))
        (if (< i (vector-length vect))
            (if (> min (vector-ref vect i))
                (loop (+ i 1) (vector-ref vect i))
                (loop (+ i 1) min))
            min))
)

(define (vector-product vect) 
    (let loop ((i 0) (prod 1))
        (if (< i (vector-length vect))
            (loop (+i 1) (* prod vector-ref vect i))
            prod))
)

(define (vector-sum vect) 
    (let loop ((i 0) (sum 0))
        (if (< i (vector-length vect))
            (loop (+ i 1) (+ sum (vector-ref vect i)))
            sum))
)

; Divides a vector by a scalar and puts the result into a second.
; The to can be the from vector.
(define (vector-divide-scalar from to scalar)
    (if (not (= (vector-length from) (vector-length to)))
        (log-fatal "The two vectors are not the same length"))

    (let loop ((i 0))
        (if (< i (vector-length from))
            (begin
                (vector-set! to i (/ (vector-ref from i) scalar))
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

