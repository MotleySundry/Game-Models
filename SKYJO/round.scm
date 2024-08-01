; Motley Sundry :: Game Models :: SKYJO :: round.scm
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

(define-structure round id players deck first-player)    

(define (new-round id)
    (let ((players (make-vector *num-players*)))

        (let loop ((i 0))
            (cond ( (< i *num-players) 
                (vector-set! players i (new-player i)) 
                (loop (+ i 1)))))

        (make-round
            id
            players ; players)
            (new-deck) ; deck
            0 ; first-player
        ))

)

(define (round-run round)
    (let ((high-player (round-deal-hands round)))
    
        (round-first-player-set! round (remainder ((+ high-player *num-players))))
    
    )
)

; Turns up two cards for each player.
; Returns the player with the highest two cards, for the first player of round 0. 
(define (round-deal-hands round)
    (define players (table-players table))
    (define (deal-card num cnt)
        (if (< cnt num)
            (let(
                (player (vector-ref players (remainder cnt num-players))))
                (s8vector-set!(player-cards player) (floor (/ cnt num-players)) (table-pop-draw-pile table))
                (deal-card num (+ cnt 1))
            )))

    ; Deal all hands
    (deal-card (* 12 num-players) 0)
    
    ; Add the first card to the discard pile.
    (table-push-discard-pile table(table-pop-draw-pile table))

    ; Return first player.
    (table-flip-two table sim-stats num-players)

)


