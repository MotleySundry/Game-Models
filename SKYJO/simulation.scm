; Motley Sundry :: Game Models :: SKYJO :: simulation.scm
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

(define-structure sim-stats num-iterations num-players)

(define (new-sim-stats)
    (make-sim-stats
        0   ;num-iterations
        0   ;num-players
    ))

(define (run-simulation sim-stats num-iterations num-players )
    (define (myfun stats num)
        (if (= num 0)
            0
            (myfun stats (- num 1))))

    (myfun sim-stats num-iterations)
    (sim-stats-num-iterations-set! sim-stats num-iterations)
    (sim-stats-num-players-set! sim-stats num-players)
)

