; Motley Sundry :: Game Models :: SKYJO :: player.scm
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

(define-structure player
    id
    round
    round-score 
    cards ; vector value -2 to 12
    card-state ;vector 0=hidden,  1=open, -1=removed
    strat ;lambda
)

; Create a new initialized player structure.
(define (new-player id round)
    ; Allocate structure
    (make-player
        id ;id
        round ;round
        0  ;round-score
        (make-vector *player-num-cards* 0) ;cards (val 0)
        (make-vector *player-num-cards* 0) ;card-state (face down)
        (get-player-strat id) ;strategy
    ))  

; Returns the index of the largest open card or #f if there are no open cards.
 (define (player-largest-open-idx player)
     (define (myfun i max-val max-idx)
        (if (= i *player-num-cards*)
            max-idx
            (if (and
                    (= (player-get-card-state player i) *card-state-open*)
                    (or
                        (not max-idx)
                        (> (player-get-card player i) max-val)
                    ))
                (myfun (+ i 1) (player-get-card player i) i)
                (myfun (+ i 1) max-val max-idx)
            ))
    )
    (myfun 0 -2 #f)
 )

;;;;;;;;;;;;;;;;;;
; PLAYER METHODS
;;;;;;;;;;;;;;;;;;

(define (player-get-deck player)
    (round-deck (player-round player))
)

(define (player-get-game player)
    (round-game (player-round player))
)

(define (player-get-round player)
    (player-round player)
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; PLAYER STRATEGY METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (player-flip-two player)
    ((player-strat player) player "flip-two") 
)

(define (player-play-phase1 player)
    ((player-strat player) player "play-phase1") 
)

(define (player-play-phase2 player)
    ((player-strat player) player "play-phase2") 
)

;;;;;;;;;;;;;;
; CARD METHODS
;;;;;;;;;;;;;;

; Returns the value of the card replaced of #f on failure.
(define (player-replace-first-hidden-card player val)
    (define (open idx)
        (if (= idx *player-num-cards*)
            (begin
                (display "!!! player-replace-first-hidden-card: there are none hidden!")
                (newline)
                (exit 1))
        
            (if (player-card_state-hidden? idx)
                (begin
                    (player-open-card! player idx)
                    (player-set-card! player idx val)
                )
                (open (+ idx 1)))))
    (open 0)
)

; Set sets the first hidden card to open 
(define (player-open-first-hidden-card! player)
    (define (open idx)
        (if (= idx *player-num-cards*)
            (log-fatal "There are no hidden files: player-open-first-hidden-card" "")
            (if (player-card-hidden? player idx)
                (player-open-card player idx)
                (open (+ idx 1)))))
    (open 0)
)
(define (player-any-cards-in-state? player state)
    (let loop ((i 0))
        (if (= i *player-num-cards*)
            #f
            (if (= (player-get-card-state player i) state)
                #t
                (loop (+ i 1)))))
)
(define (player-any-cards-open? player)
    (player-any-cards-in-state? player *card-state-open*)
)
(define (player-any-cards-hidden? player)
    (player-any-cards-in-state? player *card-state-hidden*)
)
(define (player-any-cards-removed? player)
    (player-any-cards-in-state? player *card-state-removed*)
)
(define (player-get-card-state player id)
    (vector-ref (player-card-state player) id)
)

(define (player-card-open? player id)
    (= (player-get-card-state player id) *card-state-open*)
)

(define (player-card-hidden? player id)
    (= (player-get-card-state player id) *card-state-hidden*)
)

(define (player-card-removed? player id)
    (= (player-get-card-state player id) *card-state-removed*)
)

(define (player-open-card! player id)
    (if (= (vector-ref (player-card-state player) id) *card-state-hidden*)
        (vector-set! (player-card-state player) id *card-state-open*)
        (log-fatal "Tried to open a non-hidden card: player-open-card" ""))
)

(define (player-remove-card! player id)
    (if (= (vector-ref (player-card-state player) id) *card-state-open*)
        (vector-set! (player-card-state player) id *card-state-removed*)
        (log-fatal "Tried to open a non-open card: player-remove-card" ""))
)

(define (player-get-card player id)
    (vector-ref (player-cards player) id)
)

(define (player-set-card! player id card)
    (vector-set! (player-cards player) id card)
)

; Returns the id of the highest open card or #f if there are none
(define (player-get-max-open-card player)
    (let loop ((i 0) (max-id #f) (max-value -3))
        (if (< i *player-num-cards*)
            (if (and (player-card-open? player i) (< (player-get-card player i) max-value))
                (loop (+ i 1) i (player-get-card player i))
                (loop (+ i 1) max-id max-value))
            #f))
)

; Returns the id of the first hidden card or #f if there are none
(define (player-first-hidden-card player)
    (let loop ((i 0))
        (if (= i *player-num-cards*)
            #f
            (if (player-card-hidden? player i)
                i
                (loop (+ i 1))
            )))
)

