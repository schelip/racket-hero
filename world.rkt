#lang racket/gui
(require 2htdp/universe
         "graphics.rkt"
         "chart.rkt")

;; This module controls the state of the world, rendering the appropriate graphics in the
;; correct positions based on the passing of time (song playing) and the user events

(provide create-world)

;; Wether the user is playing or just watching the computer play
(define is-playing? true)

;; WorldState is how the structure for the state during the game will be called.
;; It is either:
;;  - a list of substates
;;  - a symbol
;; During the game, it will be a list. Both the WorldState and the substates have a sctruct-like
;; behavior, naturally, since they represent a state; however, structs are not supported as
;; S-Expressions for the framework, and so, lists were used and getters were defined based on the
;; fixed positions of each substate.
;; If the game finishes, the WorldState will then be a symbol, either of sucess or failure.

;; number -> WorldState
;; Constructs the initial state of the world, with the current game-tick starting at the offset.
(define (make-world-0 offset)
  (list (list #f #f #f #f #f) ;; FingersState - List(boolean) - wether each finger is pressed
        (list 0 0 0 0 0) ;; BurnState - List(number) - burning animation remaining ticks for each lane
        (list empty ;; NotesState - List(List(number number)) - the y-position and sustain (unused)
              empty ;;                                          for each note in each lane
              empty
              empty
              empty)
        offset ;; GameTick - number - counter to spawn the charted notes
        (/ max-life 2))) ;; LifeState - number - how close the player is to losing

;; Getters for each substate of the WorldState
;; WorldState -> FingerState
(define (fingers-state state)
  (first state))
;; WorldState -> BurnState
(define (burn-state state)
  (second state))
;; WorldState -> NotesState
(define (notes-state state)
  (third state))
;; WorldState -> GameTick
(define (game-tick state)
  (fourth state))
;; WorldState -> FingerState
(define (life-state state)
  (fifth state))

;; WorldState -> WorldState
;; Applies the necessary functions to update the WorldState on every tick.
(define (update state)
  ;; Uses accumulators to apply update functions sequentially
  (let* (;; Adds notes to the lanes, based on the GameTick
         [spawned-notes (spawn-notes
                         (game-tick state)
                         (notes-state state))]
         ;; If the player is on watch-only, burns all posible notes; If playing, burn nothing
         [after-burn (if (not is-playing?)
                         (burn-all-notes (burn-state state) spawned-notes)
                         (list spawned-notes (burn-state state)))]
         ;; Moves the notes down the guitar. If too many were't burned, fail the game
         [after-note-update (update-notes (first after-burn)
                                          (life-state state))])
    (cond
      ;; Game was failed, so keep as failed
      [(symbol? after-note-update) after-note-update]
      ;; Iterates the NotesState to check if all lanes are empty; If so, the game was won.
      [(and (empty? loaded-notes) (for/and ([lane-state (first after-burn)])
                                    (empty? lane-state)))
       'game-over-success]
      ;; Since the game wasn't won or lost, update the state and move on to the next tick
      [else (list (fingers-state state) ;; No user input changes on this function
                  (update-burn (second after-burn)) ;; Pass time of the flames animation
                  (first after-note-update) ;; Notes spawned, maybe burned and moved
                  (update-game-tick (game-tick state)) ;; Increase game tick
                  (second after-note-update))]))) ;; If any notes passed, life was decreased

;; WorldState -> Scene
;; Renders the guitar based on the current state
(define (render state)
  (cond
    ;; If the game is finished, render the appropriate ending screen
    [(symbol? state) (render-game-over-screen (equal? state 'game-over-success))]
    ;; If it is not, uses higher order function 'compose' to call each rendering function sequentially.
    ;; Each function will consume the necessary state information and the previous guitar, and then provide
    ;; its own udpated guitar scene.
    [(list? state) ((compose (λ (guitar) (if is-playing?
                                             (render-lifebar (life-state state) guitar)
                                             guitar))
                             (λ (guitar) (render-notes (notes-state state) guitar))
                             (λ (guitar) (render-burn (burn-state state) guitar))
                             (λ (guitar) (render-fingers (fingers-state state) guitar)))
                    guitar)]))

;; Maps the keyboard keys to the guitar lanes
(define finger-keys
  (hash
   "a" 0
   "s" 1
   "j" 2
   "k" 3
   "l" 4))

;; boolean -> (WorldState KeyEvent -> WorldState)
;; Returns a handler for either a key press or a key release.
;; The handler  either checks if the key pressed represents a finger and then updates
;; the WorldState accordingly; or does nothing, if the user is on watch-only mode.
(define (handle-press pressing)
  (λ (state a-key)
    (if (and is-playing?
             (hash-has-key? finger-keys a-key))
        ;; Uses accumulators to get the pressed finger and apply the handler function
        (let* ([lane (hash-ref finger-keys a-key)]
               [after-burn (change-burn (fingers-state state)
                                        (burn-state state)
                                        (notes-state state)
                                        (life-state state)
                                        lane
                                        pressing)])
          ;; If the user pressed with wrong timing too many times, the game will end
          (if (symbol? after-burn)
              after-burn
              (list (change-fingers (fingers-state state)
                                    lane pressing) ;; Updates the FingersState to show visually
                    (first after-burn)
                    (second after-burn)
                    (game-tick state) ;; Stay on same game-tick
                    (third after-burn))))
        state)))

;; number symbol -> WorldState
;; Starts the world with the provided offset and play mode ('play or 'watch).
;; When it finishes, it returns the final state.
(define (create-world offset mode)
  (begin (set! is-playing? (equal? mode 'play))
         (big-bang (make-world-0 offset)
           (on-tick update)
           (to-draw render)
           (on-key (handle-press #t))
           (on-release (handle-press #f))
           (stop-when symbol? render)
           (name "Racket Hero"))))