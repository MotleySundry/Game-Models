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
(define *num-iterations* 5)
(define *game-play-bound* 150)

(define (get-player-strat id)
    (if (= id 0) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 1) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 2) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 3) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 4) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 5) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 6) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
    (if (= id 7) (lambda (player game sim-stats last-two?) (strat-naive player game sim-stats last-two?))
)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;

(define *min-players* 2)
(define *max-players* 8)

(if (> *num-players* *max-players*)
    (begin
        (display (list "Too many players:" *num-players* ))
        (exit 1)
    )
)

(if (< *num-players* *min-players*)
    (begin
        (display (list "Too few players:" *num-players* ))
        (exit 1)
    )
)

