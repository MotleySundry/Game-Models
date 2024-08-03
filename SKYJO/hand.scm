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
    card-sum        ;integer sum of the cards in the hand's hand
    card-cnt        ;integer count of the cards in the hand's hand
    open-sum        ;integer sum of the open cards in the hand's hand
    open-cnt        ;integer count of the open cards in the hand's hand
    hidden-sum      ;integer sum of the open cards in the hand's hand
    hidden-cnt      ;integer count of the open cards in the hand's hand
    removed-cnt     ;integer sum of the open cards in the hand's hand
    card-value      ;integer vector value -2 to 12
    card-state      ;integer vector 0=hidden,  1=open, -1=removed
)

; Create a new initialized hand structure.
(define (new-hand id round)
    ; Allocate structure
    (make-hand
        0                                   ;card-sum
        12                                  ;card-cnt
        0                                   ;open-sum
        0                                   ;open-cnt
        0                                   ;hidden-sum
        12                                  ;hidden-cnt
        0                                   ;removed-cnt
        (make-vector *hand-num-cards* 0)    ;card-value
        (make-vector *hand-num-cards* *card-state-hidden*)    ;card-state
    ))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND SETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hand-set-card! hand id card)
    (if ((hand-card-removed? hand id)
        (log-fatal "Tried to set the value of a removed card: hand-set-card!" id))
        (if (hand-card-open hand id)
            (hand-open-sum-set! hand (+ (hand-open-sum hand) (- card (hand-get-card hand id))))
            (hand-hidden-sum-set! hand (+ (hand-hidden-sum hand) (- card (hand-get-card hand id))))))
    (vector-set! (hand-cards hand) id card)
)

(define (hand-set-card-open! hand id)
    (if (not(hand-card-hidden? hand id))
        (log-fatal "Tried to open a non-hidden card: hand-set-card-open!" id))
        (let ((card-val (hand-get-card hand id))) 
            (hand-open-sum-set! hand (+ (hand-open-sum hand) card-val))
            (hand-open-cnt-set! hand (+ (hand-open-cnt hand) 1))
            (hand-hidden-sum-set! hand (- (hand-hidden-sum hand) card-val))
            (hand-hidden-cnt-set! hand (- (hand-hidden-cnt hand) 1))
            (vector-set! (hand-card-state hand) id *card-state-open*)
        )
)

(define (hand-set-card-removed! hand id)
    (if (not(hand-card-open? hand id))
        (log-fatal "Tried to remove a non-open card: hand-set-card-removed!" id))
        (let ((card-val (hand-get-card hand id)))
            (hand-removed-cnt-set! hand (+ (hand-removed-cnt-set hand) 1))
            (hand-open-sum-set! hand (- (hand-open-sum hand) card-val))
            (hand-open-cnt-set! hand (- (hand-open-cnt hand) 1))
            (vector-set! (hand-card-state hand) id *card-state-open*)
        )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND GETTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hand-get-card-value hand id)
    (if ((hand-card-removed? hand id)
        (log-fatal "Tried to get the value of a removed card: hand-set-card!" id)))
    (vector-ref (hand-cards hand) id)
)

(define (hand-get-card-state hand id)
    (vector-ref (hand-card-state hand) id)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND QUERIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hand-card-open? hand id)
    (= (hand-get-card-state hand id) *card-state-open*)
)

(define (hand-card-hidden? hand id)
    (= (hand-get-card-state hand id) *card-state-hidden*)
)

(define (hand-card-removed? hand id)
    (= (hand-get-card-state hand id) *card-state-removed*)
)

(define (hand-any-cards-open? hand)
    (> 1 (hand-open-cnt hand))
)

(define (hand-any-cards-hidden? hand)
    (> 1 (hand-hidden-cnt hand))
)

(define (hand-any-cards-removed? hand)
    (> 1 (hand-removed-cnt hand))
)

; Validate the hand's consistency
(define (hand-is-valid?)
    (and
    (= *hand-num-cards* (+ (hand-open-cnt hand) (hand-hidden-cnt hand) (hand-removed-cnt hand)))
    (= (hand-card-cnt hand) (+ (hand-open-cnt hand) (hand-hidden-cnt hand) )))
)


; Returns the id of the highest open card or #f if there are none
(define (hand-max-open-card hand)
    (let loop ((i 0) (max-id #f) (max-value -3))
        (if (< i *hand-num-cards*)
            (if (and (hand-card-open? hand i) (< (hand-get-card hand i) max-value))
                (loop (+ i 1) i (hand-get-card hand i))
                (loop (+ i 1) max-id max-value))
            #f))
)

; Returns the id of the first hidden card or #f if there are none
(define (hand-first-hidden-card hand)
    (let loop ((i 0))
        (if (= i *hand-num-cards*)
            #f
            (if (hand-card-hidden? hand i)
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
            (if (hand-card-hidden? hand idx)
                (hand-open-card hand idx)
                (open (+ idx 1)))))
    (open 0)
)


