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

(define *deck* '(
    -2 -2 -2 -2 -2
    0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12
    -1 1 2 3 4 5 6 7 89 10 11 12))

(define (deep-copy list)    
    (if (null? list) 
        '() 
        (if (list? list) 
            (cons (deep-copy (car list)) (deep-copy (cdr list)))
            list)))

(define-structure game
    hand-cnt deck discard players)

(define-structure player
    id score cards-value cards-up?)

(define *game* (make-game 
    0
    (deep-copy *deck*)
    '()
    (list
        (make-player 1 0 '() '())
        (make-player 2 0 '() '())
        (make-player 3 0 '() '())
        (make-player 4 0 '() '()))
    ))

(display *game*)













