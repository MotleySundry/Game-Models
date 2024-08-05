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

; Generate a random integer from 0 to (max -1) excluding an intereg value.
(define (random-integer-exclude max exclude)
    (let loop ((rnd (random-integer max)))
        (if (= rnd exclude)
            (loop (random-integer max))
            rnd))
)

; Displays everything on one line, a list is recursed
(define (print value)
    (let loop ((val value))
        (if (not (null? val))
                (if (list? val)
                    (begin (loop (car val)) (loop (cdr val)))
                    (begin (display val)(display " ")))))
    (newline)
)

; Displays values separate lines, a list is recursed
(define (println value)
    (let loop ((val value))
        (if (not (null? val))
                (if (list? val)
                    (begin (loop (car val)) (loop (cdr val)))
                    (begin (display val)(newline)))))
)

(define (log-fatal msg data)
    (display "### ")(display msg)(display " ### ")(display (time->seconds(current-time)))(newline)
    (println data)
    (exit 1)
)

(define (log-error msg data)
    (display "!!! ")(display msg)(display " !!! ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)

(define (log-warning msg data)
    (display "=== ")(display msg)(display " === ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)

(define (log-info msg data)
    (display "--- ")(display msg)(display " --- ")(display (time->seconds(current-time)))(newline)
    (println data)
    data
)



