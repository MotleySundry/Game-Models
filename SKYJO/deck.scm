; Motley Sundry :: Game Models :: SKYJO :: deck.scm
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

(define-structure deck draw-pile discard-pile draw-cnt draw-sum discard-cnt discard-sum)

(define (new-deck)
    (make-deck
        (vector-to-list (vector-rand (vector-dup *deck*)))  ;draw-pile
        '()         ;discard-pile
        *deck-size* ;draw-cnt
        *deck-sum*  ;draw-sum
        0           ;discard-cnt
        0)          ;discard-sum
)

; Removes the top card on the draw pile.
; Returns the value of the card or #f if the draw pile is empty.
(define (deck-pop-draw-pile! deck)
    ; Draw pile empty?
    (if (null? (deck-draw-pile deck))
        (log-fatal "The draw pile is empty: deck-pop-draw-pile!" "")
    
        ; Pop it off
        (let ((card (car (deck-draw-pile deck))))
            (deck-draw-pile-set! deck (cdr (deck-draw-pile deck)))
            (deck-draw-cnt-set! deck (- (deck-draw-cnt deck) 1))
            (deck-draw-sum-set! deck (- (deck-draw-sum deck) card))
            card
        ))
)

; Removes the top card on the discard pile.
; Returns the value of the card or #f if the discard pile is empty.
(define (deck-pop-discard-pile! deck)
    ; Discard pile empty?
    (if (null? (deck-discard-pile deck))
        (log-fatal "The discard pile is empty: deck-pop-discard-pile!" "")
    
        ; Pop it off
        (let ((card (car (deck-discard-pile deck))))
            (deck-discard-pile-set! deck (cdr (deck-discard-pile deck)))
            (deck-discard-cnt-set! deck (- (deck-discard-cnt deck) 1))
            (deck-discard-sum-set! deck (- (deck-discard-sum deck) card))

            card
        ))
)

; Returns the value of the top card on the discard pile.
; Returns #f if the discard pile is empty.
(define (deck-discard-value deck)
    (if (null? (deck-discard-pile deck))
        (log-fatal "The discard pile is empty: deck-pop-discard-pile!" "")
        (car (deck-discard-pile deck)))
)

; Places the value of the card onto the discard pile.
(define (deck-push-discard-pile! deck card)
    (deck-discard-pile-set! deck (cons card (deck-discard-pile deck)))
    (deck-discard-cnt-set! deck (+ (deck-discard-cnt deck) 1))
    (deck-discard-sum-set! deck (+ (deck-discard-sum deck) card))
    #t
)

; DECK GETTERS
(define (deck-get-draw-mean deck)
    (/ (+ 0.0 (deck-draw-sum deck)) (+ 0.0 (deck-draw-cnt deck)))
)

(define (deck-get-draw-median deck)
    (list-get-median (deck-draw-pile deck)))


; DECK PRINT
(define (deck-print deck tab)
    (display tab)(print "--- Deck ---")
    (display tab)(print (list "Draw Pile cnt,sum,mean,median:   " 
        (deck-draw-cnt deck) "," 
        (deck-draw-sum deck) "," 
        (deck-get-draw-mean deck) "," 
        (deck-get-draw-median deck)))
    (display tab)(print (list "Disc Pile cnt,sum:   " (deck-discard-cnt deck) "," (deck-discard-sum deck)))
    (newline)
)

