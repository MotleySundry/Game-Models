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
    card-up    ; 0 - face down 1 - face up
    strategy   ; procedure
)

; Create a new initialized player structure.
(define (new-player id strat)
    (make-player
        id  ;id
        0   ;score
        (make-s8vector 12 99)   ;cards
        (make-s8vector 12 0)    ;card-up
        strat          ;strategy
    ))  