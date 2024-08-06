; Motley Sundry :: Game Models :: matrix.scm
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

; Functions for an n-dimensional matrix.

(define-structure matrix
    rowwise? ; #t if the matrix is stored rowwise, #f is it is stored columnwise
    size ; integer vector containing size of each dimension
    data ; number vector containing the elements
)

(define (new-matrix size #!optional(init #f) #!key (rowwise? #t))
    (let ((len (vector-product size)))
            (make-matrix
            rowwise?
            size
            (if init (make-vector len init) (make-vector len))))
)
