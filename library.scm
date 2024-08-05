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

; Prints all elements with an optional separator.
(define (print #!rest r #!key (sep #f))
    (if r 
        (let loop ((lst r))
            (if (not (null? lst))
                (begin 
                    (display (car lst)) 
                    (if (and sep (not (null? (cdr lst)))) (display sep))
                    (loop (cdr lst))))))
)

; Prints all elements on a line with an optional separator.
(define (println #!rest r #!key (sep #f))
    (if r 
        (let loop ((lst r))
            (if (not (null? lst))
                (begin 
                    (display (car lst)) 
                    (if (and sep (not (null? (cdr lst)))) (display sep))
                    (loop (cdr lst))))))
    (newline)
)

; Prints all elements of the list on separate lines, with an optional prefix.
(define (print-list lst #!optional prefix)
    (let loop ((lst lst))
        (if (not (null? lst))
            (begin 
                (if prefix (display prefix))
                (display (car lst)) 
                (newline)
                (loop (cdr lst)))))

)

(define (log-fatal msg #!rest data)
    (println "### " msg " ### " (time->seconds(current-time)))
    (if data (print-list data "  "))
    (exit 1)
)

(define (log-error msg #!rest data)
    (println "!!! " msg " !!! " (time->seconds(current-time)))
    (if data (print-list data "  "))
)

(define (log-warning msg #!rest data)
    (println "=== " msg " === " (time->seconds(current-time)))
    (if data (print-list data "  "))
)

(define (log-info msg #!rest data)
    (println "--- " msg " --- " (time->seconds(current-time)))
    (if data (print-list data "  "))
)



