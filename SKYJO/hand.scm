; Motley Sundry :: Game Models :: SKYJO :: hand.scm
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

; !!! DO NOT CHANGE OR READ THESE DIRECTLY !!!
; !!! USE THE HAND FUNCTIONS INSTEAD !!!
(define-structure hand
    ; Updated during each play, only change with setters!
    card-value      ;integer vector -2 to 12; 
                    ;Index: 0=lower-left, 11=upper-right
    card-state      ;integer vector 0=hidden, 1=open, -1=removed
                    ;Index: 0=lower-left, 11=upper-right
)

; Create a new initialized hand structure.
(define (new-hand)
    ; Allocate structure
    (make-hand
        (make-vector *hand-num-cards* 0)    ;card-value
        (make-vector *hand-num-cards* *card-state-hidden*)    ;card-state
    ))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND SETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Open all hidden cards
(define (hand-open-all-hidden-cards! hand)
    (let loop ((i 0))
        (if (< i *hand-num-cards*)
            (begin 
                (if (hand-is-card-hidden? hand i) (hand-set-card-open! hand i))
                (loop (+ i 1)))))
)

(define (hand-remove-matching-columns! hand deck)
    (+ (hand-remove-matching-columns-by-ids! hand deck 0 4 8)
        (hand-remove-matching-columns-by-ids! hand deck 1 5 9)
        (hand-remove-matching-columns-by-ids! hand deck 2 6 10)
        (hand-remove-matching-columns-by-ids! hand deck 3 7 11))
)

(define (hand-remove-matching-columns-by-ids! hand deck id1 id2 id3)
    (if (and
        (hand-is-card-open? hand id1)
        (hand-is-card-open? hand id2)
        (hand-is-card-open? hand id3))
        (let (
            (val1 (hand-get-card-value hand id1))
            (val2 (hand-get-card-value hand id2))
            (val3 (hand-get-card-value hand id3)))
            
            (if (and (< 0 val1) (= val1 val2 val3))
                (begin
                    (log-debug 2 "Removed column: " id1 id2 id3)
                    (hand-set-card-removed! hand id1)
                    (hand-set-card-removed! hand id2)
                    (hand-set-card-removed! hand id3)

                    (deck-push-discard-pile! deck val1)
                    (deck-push-discard-pile! deck val2)
                    (deck-push-discard-pile! deck val3)
                    1)
                0))
        0)
)

(define (hand-set-card-value! hand id card)
    (if (hand-is-card-removed? hand id)
        (log-fatal "Tried to set the value of a removed card: hand-set-card!" (list id hand))
    (vector-set! (hand-card-value hand) id card))
)

(define (hand-set-card-open! hand id)
    (if (not(hand-is-card-hidden? hand id))
        (log-fatal "Tried to open a non-hidden card: hand-set-card-open!" id ))
    (vector-set! (hand-card-state hand) id *card-state-open*)
)


(define (hand-set-card-removed! hand id)
    (if (not(hand-is-card-open? hand id))
        (log-fatal "Tried to remove a non-open card: hand-set-card-removed!" id))
    (vector-set! (hand-card-state hand) id *card-state-open*)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND GETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hand-get-card-value hand id)
    (if (hand-is-card-removed? hand id)
        (log-fatal "Tried to get the value of a removed card: hand-get-card-value" id)
        (vector-ref (hand-card-value hand) id))
)

(define (hand-get-card-state hand id)
    (vector-ref (hand-card-state hand) id)
)

(define (hand-random-card-state-id hand state)
    (define n (vector-count-values (hand-card-state hand) state ))
    (if (< n 1)
         (log-fatal "There are no cards in this state: hand-random-card-state-id:" state))
    (vector-get-value-idx (hand-card-state hand) state (random-integer n))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND QUERIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hand-total hand)
    (let loop ((i 0) (sum 0))
        (if (< i *hand-num-cards*)
            (if (not (= (vector-ref (hand-card-state hand) i) *card-state-removed*))
                (loop (+ i 1) (+ sum (vector-ref (hand-card-value hand) i)))
                (loop (+ i 1) sum))
        sum))
)

(define (hand-sum hand card-state)
    (let loop ((i 0) (sum 0))
        (if (< i *hand-num-cards*)
            (if (= (vector-ref (hand-card-state hand) i) card-state)
                (loop (+ i 1) (+ sum (vector-ref (hand-card-value hand) i)))
                (loop (+ i 1) sum))
        sum))
)

(define (hand-cnt hand card-state)
    (let loop ((i 0) (cnt 0))
        (if (< i *hand-num-cards*)
            (if (= (vector-ref (hand-card-state hand) i) card-state)
                (loop (+ i 1) (+ cnt 1))
                (loop (+ i 1) cnt))
        cnt))
)



(define (hand-is-card-open? hand card-id)
    (= (hand-get-card-state hand card-id) *card-state-open*)
)

(define (hand-is-card-hidden? hand card-id)
    (= (hand-get-card-state hand card-id) *card-state-hidden*)
)

(define (hand-is-card-removed? hand card-id)
    (= (hand-get-card-state hand card-id) *card-state-removed*)
)

(define (hand-any-cards-open? hand)
    (> (hand-cnt hand *card-state-open*) 0)
)

(define (hand-any-cards-hidden? hand)
    (> (hand-cnt hand *card-state-hidden*) 0)
)

(define (hand-any-cards-removed? hand)
    (> (hand-cnt hand *card-state-removed*) 0)
)

; Returns the idx of the position where a card with value will create a tripple, #f otherwise.
(define (hand-complete-column-card-idx hand value)
    (let loop-col ((col 0))
        (if (< col 4)
            (let ((complete-idx (col-complete-row hand col value)))
                (if complete-idx
                    complete-idx
                    (loop-col (+ col 1))))
            #f)
    )
)

; Returns #t if the card at idx matches value and is open, otherwise #f.
(define (card-matches? value cs cv idx)
    (and 
        (= *card-state-open* (vector-ref cs idx))
        (= value (vector-ref cv idx)))
)

; Returns the idx where a card of value will create a tripple in col, otherwise #f.
(define (col-complete-row hand col value)
    (define cs (hand-card-state hand))
    (define cv (hand-card-value hand))
    (define base (* col 3))
    (cond
        ((and (card-matches? value cs cv (+ base 0)) (card-matches? value cs cv (+ base 1)))
         (+ base 2))
        ((and (card-matches? value cs cv (+ base 0)) (card-matches? value cs cv (+ base 2)))
         (+ base 1))
        ((and (card-matches? value cs cv (+ base 1)) (card-matches? value cs cv (+ base 2)))
         (+ base 0))
        (else #f))
)

(define (hand-value-estimate hand)
    (+ (hand-sum-open-cards hand) (* 5 (hand-cnt hand *card-state-hidden*)))
)

(define (hand-sum-open-cards hand)
    (hand-sum hand *card-state-open*)
)

; Returns the sum of the cards in state
(define (hand-get-sum-of-cards-in-state hand state)
    (let loop ((i 0) (sum 0))
        (if (< i *hand-num-cards*)
            (if (= state (hand-get-card-state hand i))
                (loop (+ i 1) (+ sum (hand-get-card-value hand i)))
                (loop (+ i 1) sum)))
        sum)
)

(define (hand-get-highest-open-card hand  #!optional exclude)
    (hand-get-highest-card-in-state hand *card-state-open* exclude)  
)

(define (hand-get-highest-hidden-card hand #!optional exclude)
    (hand-get-highest-card-in-state hand *card-state-hidden* exclude)  
)

; Returns the id of the highest card in state or #f if there are none
; The first highest card found wins.
(define (hand-get-highest-card-in-state hand state #!optional exclude)
    (let loop ((i 0) (max-id #f) (max-value -3))
        (if (< i *hand-num-cards*)
            (if (and
                    (= state (hand-get-card-state hand i))
                    (not (and exclude (= i exclude)))
                    (> (hand-get-card-value hand i) max-value))
                (loop (+ i 1) i (hand-get-card-value hand i))
                (loop (+ i 1) max-id max-value))
            max-id))
)

; Returns the id of the first card in state or #f if there are none
(define (hand-get-first-card-in-state hand state)
    (let loop ((i 0))
        (if (= i *hand-num-cards*)
            #f
            (if ((= state (hand-get-card-state hand i)))
                i
                (loop (+ i 1))
            )))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND MODIFIERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns the value of the card replaced or #f on failure.
(define (hand-replace-first-hidden-card! hand val)
    (define (open idx)
        (if (= idx *hand-num-cards*)
            (begin
                (display "!!! hand-replace-first-hidden-card: there are none hidden!")
                (newline)
                (exit 1))
        
            (if (hand-card-hidden? idx)
                (begin
                    (hand-open-card! hand idx)
                    (hand-set-card! hand idx val)
                )
                (open (+ idx 1)))))
    (open 0)
)

; Set sets the first hidden card to open 
(define (hand-open-first-hidden-card! hand)
    (define (open idx)
        (if (= idx *hand-num-cards*)
            (log-fatal "There are no hidden files: hand-open-first-hidden-card" "")
            (if (hand-is-card-hidden? hand idx)
                (hand-set-card-open! hand idx)
                (open (+ idx 1)))))
    (open 0)
)

; HAND PRINT
(define (hand-print hand tab)
    (ptintln tab "--- Hand ---")
    (ptintln tab (list "card-value:" (hand-card-value hand)))
    (ptintln tab (list "card-state:" (hand-card-state hand)))
    (ptintln tab (list "hidden  cnt,sum:" (hand-hidden-cnt hand) "," (hand-hidden-sum hand)))
    (ptintln tab (list "open    cnt,sum:" (hand-open-cnt hand) "," (hand-open-sum hand)))
    (ptintln tab (list "removed cnt:    " (hand-removed-cnt hand)))
    (newline)
)

