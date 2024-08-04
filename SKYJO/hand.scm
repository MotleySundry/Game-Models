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
(define (new-hand)
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

(define (hand-set-card-value! hand id card)
    (let ((old-val (hand-get-card-value hand id)))
        (if (hand-is-card-removed? hand id)
            (log-fatal "Tried to set the value of a removed card: hand-set-card!" (list id hand))
            (if (hand-is-card-open? hand id)
                (hand-open-sum-set! hand (+ (hand-open-sum hand) (- card old-val )))
                (hand-hidden-sum-set! hand (+ (hand-hidden-sum hand) (- card old-val )))))
        (hand-card-sum-set! hand (+ (hand-card-sum hand) (- card old-val )))
        (vector-set! (hand-card-value hand) id card))
)

(define (hand-set-card-open! hand id)
    (if (not(hand-is-card-hidden? hand id))
        (log-fatal "Tried to open a non-hidden card: hand-set-card-open!" id ))
        (let ((card-val (hand-get-card-value hand id))) 
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
        (let ((card-val (hand-get-card-value hand id)))
            (hand-removed-cnt-set! hand (+ (hand-removed-cnt-set hand) 1))
            
            (hand-card-sum-set! hand (- (hand-card-sum hand) card-val))
            (hand-card-cnt-set! hand (- (hand-card-cnt hand) 1))

            (hand-open-sum-set! hand (- (hand-open-sum hand) card-val))
            (hand-open-cnt-set! hand (- (hand-open-cnt hand) 1))
            
            (vector-set! (hand-card-state hand) id *card-state-open*)
        )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;               HAND QUERIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (> 1 (hand-open-cnt hand))
)

(define (hand-any-cards-hidden? hand)
    (> 1 (hand-hidden-cnt hand))
)

(define (hand-any-cards-removed? hand)
    (> 1 (hand-removed-cnt hand))
)

; Validate the hand's consistency
(define (hand-is-valid? hand)
    (define test-num 0)

    (if (not (= *hand-num-cards* (+ (hand-open-cnt hand) (hand-hidden-cnt hand) (hand-removed-cnt hand))))
        (log-fatal "Hand valid test failed" test-num)
        (set! test-num (+ test-num 1)))
        
    (if (not (= (hand-card-cnt hand) (+ (hand-open-cnt hand) (hand-hidden-cnt hand))))
        (log-fatal "Hand valid test failed" test-num)
        (set! test-num (+ test-num 1)))
    
    (if (not (= (hand-card-sum hand) (+ (hand-open-sum hand) (hand-hidden-sum hand))))
        (log-fatal "Hand valid test failed" test-num)
        (set! test-num (+ test-num 1)))

    #t       
)

; Returns the id of the highest open card or #f if there are none
(define (hand-get-largest-open-card hand)
    (let loop ((i 0) (max-id #f) (max-value -3))
        (if (< i *hand-num-cards*)
            (if (and (hand-is-card-open? hand i) (< (hand-get-card-value hand i) max-value))
                (loop (+ i 1) i (hand-get-card-value hand i))
                (loop (+ i 1) max-id max-value))
            #f))
)

; Returns the id of the first hidden card or #f if there are none
(define (hand-get-first-hidden-card hand)
    (let loop ((i 0))
        (if (= i *hand-num-cards*)
            #f
            (if (hand-is-card-hidden? hand i)
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

; HAND PRINT
(define (hand-print hand tab)
    (display tab)(print "--- Hand ---")
    (display tab)(print (list "card-value: " (hand-card-value hand)))
    (display tab)(print (list "card-state: " (hand-card-state hand)))
    (newline)
)

