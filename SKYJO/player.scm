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

(define *card-state-hidden* 0)
(define *card-state-open* 1)
(define *card-state-removed -1)

(define-structure player
    id
    round
    round-score 
    cards ; s8vector value -2 to 12
    card-state ;s8vector 0=hidden,  1=open, -1=removed
    strat ;lambda
)

; Create a new initialized player structure.
(define (new-player id round)
    ; Allocate structure
    (make-player
        id ;id
        round ;round
        0  ;round-score
        (make-s8vector *player-num-cards* 0) ;cards (val 0)
        (make-s8vector *player-num-cards* 0) ;card-state (face down)
        (get-player-strat id) ;strategy
    ))  

; Returns the count if the cards in state or #f for failure.
(define (player-count-cards-in-state? player state)
    (define (myfun i cnt)
        (if (= i *player-num-cards*)
            cnt
            (if (= (s8vector-ref (player-card-state player) i) state)
                (myfun (+ i 1) (+ cnt 1))
                (myfun (+ i 1) cnt)
            ))
    )
    (myfun 0 0)
)

; Returns the index of the largest open card or #f if there are no open cards.
 (define (player-largest-open-idx player)
     (define (myfun i max-val max-idx)
        (if (= i *player-num-cards*)
            max-idx
            (if (and
                    (= (s8vector-ref (player-card-state player) i) *card-state-open*)
                    (or
                        (not max-idx)
                        (> (s8vector-ref (player-cards player) i) max-val)
                    ))
                (myfun (+ i 1) (s8vector-ref (player-cards player) i) i)
                (myfun (+ i 1) max-val max-idx)
            ))
    )
    (myfun 0 -2 #f)
 )

; Returns the value of the card replaced of #f on failure.
(define (player-replace-first-hidden-card player val)
    (define (open idx)
        (if (= idx *player-num-cards*)
            (begin
                (display "!!! player-replace-first-hidden-card: there are none hidden!")
                (newline)
                (exit 1))
        
            (if (= (s8vector-ref(player-card-state player) idx) *card-state-hidden*)
                (begin
                (player-set-card-state player idx *card-state-open*)
                (player-set-card player idx val)
                )
                (open (+ idx 1)))))
    (open 0)
)

; Replaces the value and opens the card state.
(define (player-replace-card player idx val)
    (s8vector-set!(player-cards player) idx val)
    (player-set-card-state player idx *card-state-open*)
)

; Sets the card state
(define (player-set-card-state player idx state)
    (s8vector-set! (player-card-state player) idx state)
)

; Sets the card value
(define (player-set-card player idx card)
    (s8vector-set! (player-cards player) idx card)
)

(define (player-any-hidden-cards? player)
  (< 0 (player-count-cards-in-state? player *card-state-hidden*))
)

(define (player-any-open-cards? player)
  (< 0 (player-count-cards-in-state? player *card-state-open*))
)


; Set sets the first hidden card to open 
(define (player-open-first-hidden-card player)
    (define (open idx)
        (if (= idx *player-num-cards*)
            (begin
                (display "!!! player-open-first-hidden-card: there are none hidden!")
                (newline)
                (exit 1))
        
            (if (= (s8vector-ref(player-card-state player) idx) *card-state-hidden*)
                (player-set-card-state player idx *card-state-open*)
                (open (+ idx 1)))))

    (open 0)
)

(define (player-flip-two player)
    ((player-strat player) player "flip-two") 
)

(define (player-play-phase1 player)
    ((player-strat player) player "play-phase1") 
)

(define (player-play-phase2 player)
    ((player-strat player) player "play-phase2") 
)

; PLAYER ACCESSORS

(define (player-get-card player id)
    (vector-ref (player-cards player) id)
)

(define (player-set-card! player id card)
    (vector-set! (player-cards player) id card)
)

(define (player-get-deck player)
    (round-deck (player-round player))
)

(define (player-get-game player)
    (round-game (player-round player))
)

(define (player-get-round player)
    (player-round player)
)
