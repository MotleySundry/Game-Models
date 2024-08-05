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

(define (new-vector2 num-vectors vector-size #!optional init)
    (let ((v2 (make-vector num-vectors)))
        (let loop ((i 0))
            (if (< i num-vectors)
                (begin
                    (vector-set! v2 i ( if init 
                        (make-vector vector-size init)
                        (make-vector vector-size)))
                    (loop (+ i 1)))
                v2)))
)