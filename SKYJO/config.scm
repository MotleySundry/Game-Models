; Motley Sundry :: Game Models :: SKYJO :: config.scm
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


(define *num-players* 4)
(define *num-runs* 10)

(define *strategies* (list
    strat-naive
    strat-naive
    strat-naive
    strat-naive
    strat-naive
    strat-naive
    strat-naive
    strat-naive
))

;;;;;;;;;;;;;;;;;;;;;;;;

(define *min-playerst* 2)
(define *max-playerst* 8)
