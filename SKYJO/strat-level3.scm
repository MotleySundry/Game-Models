; Motley Sundry :: Game Models :: SKYJO :: strat-level3.scm
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

; Returns #f when the last card is turned over.
;
; ========================
; ===  Level3 strategy ===
; ========================
; The third tier strategy that adds game-level strategy on top of level-2 round-level strategy.
; For each round it sets player parameters to match the game properties.
;

(define (strat-level3 player cmd)

    (cond
        ; Returns the string label of the strategy or #f on failure.
        ((= cmd *strat-cmd-get-label*) "Level-3")

        ; Returns #t if a play was executed or #f otherwise
        ((= cmd *strat-cmd-play-phase1*)
                (strat-level3-phase1 player))
        
        ; Returns #t if a play was executed or #f otherwise.
        ((= cmd *strat-cmd-play-phase2*)
                (strat-level2-phase2 player))

        ; Returns a list of the card ids or #f otherwise.
        ((= cmd *strat-cmd-flip-two*)
            (strat-level2-flip-two player))
        
        (else
            (display "Unknown command: ")
            (display cmd)
            (newline)
            (exit 1)))
)

(define (strat-level3-phase1 player)

    (define round-cnt      (player-api-round-cnt player))
    (define my-hand        (player-api-my-hand-value-estimate player))
    (define my-points      (player-api-my-current-game-points player))
    (define lowest-hand    (player-api-lowest-opponent-value-estimate player))
    (define lowest-points  (player-api-lowest-opponent-current-game-score player))

    (define margin 
        (cond
            ((= round-cnt 0) 0)
            ((= round-cnt 1) 0)
            ((= round-cnt 2) 0)
            ((= round-cnt 3) 0)
            ((= round-cnt 4) 0)
            (else 0)
    ))

    (log-debug 2 "strat-level3-phase1"
        "player =" (player-id player)
        "round-cnt =" round-cnt  
        "my-hand =" my-hand     
        "my-points =" my-points     
        "lowest-hand =" lowest-hand     
        "lowest-points =" lowest-points
        "margin =" margin)

    (player-terminate-round-margin-set! player margin)
    (strat-level2-phase1 player)
)

