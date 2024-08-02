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

(define-structure deck draw-pile discard-pile)

(define (new-deck)
    (make-deck
        (vector-to-list (vector-rand (vector-dup *deck*)))  ;draw-pile
        '()) ; discard-pile
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
    #t
)
