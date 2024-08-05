; Motley Sundry :: Game Models :: matrix2.scm
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

; A sparse two-dimensional vector of vectors
(define-structure vector2 num-rows row-len rows)

(define (new-vector2 num-rows row-len #!optional init)
    (define rows (make-vector num-rows #f))
    (define vect2 (make-vector2 num-rows row-len rows))
    (if init 
        (let loop ((i 0))
            (if (< i num-rows)
                (begin
                    (vector-set! rows (make-vector row-length init))
                    (loop (+ i 1))))))
    vect2
)

(define (vector2-row-set! vect2 row data)
    (cond 
        ((vector? data)
            (if (not (= (vector-length data) (vector2-row-len vect2)))
                (log-fatal "The vect2 length does not match: vector2-set-row!"
                    (vector-length data) (vector2-row-len vect2))
                (vector-set! (vector2-rows vect2) row (vector-dup data))))

        (else
            (vector-set! (vector2-rows vect2) row (make-vector (vector2-row-len vect2) data))))
)

(define (vector2-insure-row! vect2 row #!optional init)
    (if (not (vector-ref (vector2-rows vect2) row))
        (if init
            (vector-set! (vector2-rows vect2) row (make-vector (vector2-row-len) init))
            (vector-set! (vector2-rows vect2) row (make-vector (vector2-row-len)))))
)
