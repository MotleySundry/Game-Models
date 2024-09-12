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
    id          ;integer player id, unique in context
    round       ;reference to the containing round
    hand        ;reference to the players hand
    strat       ;lambda reference to the assigned strategy
    points      ;integer points the player scored for the round, updated at the end of the round
    removed     ;integer count of uni-value columns removed
    penalties   ;integer penalty points for going out first and too high
    plays       ;integer number of plays
)

; Create a new initialized player structure.
(define (new-player id round)
    ; Allocate structure
    (make-player
        id                      ;id
        round                   ;round
        (new-hand)              ;hand
        (get-player-strat id)   ;strat
        0                       ;points
        0                       ;removed
        0                       ;penalties
        0                       ;plays
    ))  

;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLAYER STRATEGY COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Returns the string label of the strategy or #f on failure.
(define (player-get-strat-label player)
    ((player-strat player) player *strat-cmd-get-label*) 
)

; Returns the sun of the cards
(define (player-flip-two player)
    (define resp ((player-strat player) player *strat-cmd-flip-two*))

    (if (or (not (car resp))(not (cadr resp)))
        (log-fatal "A card id is false in the response: player-flip-two player" 
            resp (player-get-strat-label player)))

    (hand-set-card-open! (player-hand player) (car resp))
    (hand-set-card-open! (player-hand player) (cadr resp))

    (+ (hand-get-card-value (player-hand player) (car resp))
        (hand-get-card-value (player-hand player) (cadr resp)))
)

; Returns #f if phase1 ended
(define (player-play-phase1 player)
    (if (not ((player-strat player) player *strat-cmd-play-phase1*))
        (log-fatal "Player failed to make a play: play-phase1" 
            (player-id player) (player-get-strat-label player)))

    (player-plays-set! player (+ 1 (player-plays player)))
    (player-remove-matching-columns! player)
    (hand-any-cards-hidden? (player-hand player))
)

(define (player-play-phase2 player)
    (if (not ((player-strat player) player *strat-cmd-play-phase2*))
        (log-fatal "Player failed to make a play: play-phase2" 
            (player-id player) (player-get-strat-label player)))
    (player-plays-set! player (+ 1 (player-plays player)))
    (hand-open-all-hidden-cards! (player-hand player))
    (player-remove-matching-columns! player)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLAYER STRATEGY LEGAL CALLBACKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A legal strategy should only use these calls,

(define (player-hand-value-estimate player)
    (hand-value-estimate (player-hand player))
)
    
(define (player-lowest-opponent-value-estimate player)
    (define players (round-players (player-round player)))
    (define my-id (player-id player))
    (loop ((i 0) (low 13))
        (if (< i *num-players*)
            (let ((estimate (player-hand-value-estimate (vector-ref players i))))
                (if (and (< estimate low) (not (= i my-id)))
                    (loop (+ i 1) estimate)
                    (loop (+ i 1) low))
            ))
        low
    )
)


(define (player-api-complete-column-card-idx player value)
    (hand-complete-column-card-idx (player-hand player) value)
)

; Returns a random hidden card id or #f on failure
(define (player-api-random-hidden-card-id player)
    (hand-random-card-state-id (player-hand player) *card-state-hidden*)
)

(define (player-api-get-open-card-value player card-id)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-get-open-card-value" (player-get-strat-label player)))
    (if (not (hand-is-card-open? (player-hand player) card-id))
        (log-fatal "The card is not open: player-get-open-card-value"
            (player-api-get-strat-label player))
    (hand-get-card-value (player-hand player) card-id))
)

(define (player-api-draw-card player)
    (deck-pop-draw-pile! (round-deck (player-round player)))
)
    
(define (player-api-any-cards-open? player)
    (hand-any-cards-open? (player-hand player))
)

(define (player-api-num-cards-open player)
    (hand-cnt (player-hand player) *card-state-open*)
)

(define (player-api-num-cards-hidden player)
    (hand-cnt (player-hand player) *card-state-hidden*)
)

(define (player-api-any-cards-hidden? player)
    (hand-any-cards-hidden? (player-hand player))
)

(define (player-api-any-cards-removed? player)
    (hand-any-cards-removed? (player-hand player))
)

(define (player-api-card-open? player card-id)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-card-open?" (player-get-strat-label player)))
    (hand-is-card-open? (player-hand player) card-id)
)

(define (player-api-card-hidden? player card-id)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-card-hidden?" (player-get-strat-label player)))
    (hand-is-card-hidden? (player-hand player) card-id)
)

(define (player-api-card-removed? player card-id)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-card-hidden?" (player-get-strat-label player)))
    (hand-is-card-removed? (player-hand player) icard-idd)
)

; Returns the index of the highest open card or #f if there are no open cards.
(define (player-api-get-highest-open-card-idx player)
    (hand-get-highest-open-card (player-hand player ))
) 

(define (player-api-get-discard-val player)
    (deck-discard-top-card-val (round-deck (player-round player)))
)

; Replaces a players card with a card-value.
; It is used for a card that has been taken from the draw pile.
(define (player-api-replace-card-with-draw-card! player card-id card-value)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-replace-card-with-draw-card!" (player-get-strat-label player)))
    (deck-push-discard-pile! (round-deck (player-round player)) 
        (hand-get-card-value (player-hand player) card-id))
    (hand-set-card-value! (player-hand player) card-id card-value)
    (if(hand-is-card-hidden? (player-hand player) card-id)
        (hand-set-card-open! (player-hand player) card-id)) 
)

; Replaces a players card by popping the top card off the discard pile.
; The discard top is viewable by the strategy so it is popped here.
(define (player-api-replace-card-from-discard! player card-id)
    (if (not card-id)
        (log-fatal "Card Id is false: player-api-replace-card-from-discard!" (player-get-strat-label player)))
    (let ((card-value (deck-pop-discard-pile! (round-deck (player-round player)))))
        (deck-push-discard-pile! (round-deck (player-round  player)) 
            (hand-get-card-value (player-hand player) card-id))
        (hand-set-card-value! (player-hand player) card-id card-value)
        (if(hand-is-card-hidden? (player-hand player) card-id)
            (hand-set-card-open! (player-hand player) card-id)))
)

; Discard a card-value
; It is used for a card that has been taken from the draw pile.
(define (player-api-discard-draw-card! player card-value hidden-id)
    (if (not hidden-id)
        (log-fatal "Hidden Id is false: player-api-discard-draw-card" (player-get-strat-label player)))
    (deck-push-discard-pile! (round-deck (player-round player)) card-value)
    (hand-set-card-open! (player-hand player) hidden-id)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PLAYER STRATEGY CHEAT CALLBACKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A cheating strategy can use these calls,

(define (player-cheat-get-card-value player card-id)
    (if (not *cheating-allowed?*)
        (log-fatal "Cheating is not allowed: player-cheat-get-card-value"
            (player-get-strat-label player))
    (hand-get-card-value (player-hand player) card-id))
)

(define (player-cheat-next-draw-card-val player)
    (if (not *cheating-allowed?*)
        (log-fatal "Cheating is not allowed: player-cheat-next-draw-card-val"
            (player-get-strat-label player))
    (deck-cheat-next-draw-card-val (round-deck (player-round player))))
)

 (define (player-cheat-get-highest-hidden-card player #!optional exclude)
     (if (not *cheating-allowed?*)
        (log-fatal "Cheating is not allowed: player-cheat-get-largest-hidden-card"
            (player-get-strat-label player))
    (hand-get-highest-hidden-card (player-hand player) exclude))
 )

; PLAYER PRINT
(define (player-print player tab)
    (ptintln tab "--- Player ---")
    (ptintln tab (list "id:      " (player-id player)))
    (ptintln tab (list "Strategy:" (player-strat-label player)))
    (hand-print (player-hand player) (string-append tab "  "))
    (newline)
)

(define (player-print-round player tab)
    (round-print (player-round player) "")
)

(define (player-remove-matching-columns! player)
    (define removed (hand-remove-matching-columns! (player-hand player) (round-deck (player-round player))))
    (player-removed-set! player (+ (player-removed player) removed ))
)
