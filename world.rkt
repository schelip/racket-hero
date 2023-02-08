#lang racket/gui
(require 2htdp/universe
         "graphics.rkt"
         "chart.rkt")

;; This module controls the state of the world, rendering the appropriate graphics in the
;; correct positions based on the passing of time (song playing) and the user events

(provide create-world)

;; ---------------------------------------------------------------------------------------------------
;; WORLD
;; ---------------------------------------------------------------------------------------------------

(define is-playing? true)

;; initial state of the world
(define (make-world-0 offset)
  (list (list #f #f #f #f #f) ;; pressed fingers
        (list 0 0 0 0 0) ;; burning animations
        (list empty ;; notes in each lane
              empty
              empty
              empty
              empty)
        offset
        (/ max-life 2)))

;; getters for each part of the world state
(define (fingers-state state)
  (first state))
(define (burn-state state)
  (second state))
(define (notes-state state)
  (third state))
(define (game-tick state)
  (fourth state))
(define (life-state state)
  (fifth state))

;; moves the notes every clock tick
(define (update state)
  (let* ([spawned-notes (spawn-notes
                         (game-tick state)
                         (notes-state state))]
         [after-burn (if (not is-playing?)
                         (burn-all-notes (burn-state state) spawned-notes)
                         (list spawned-notes (burn-state state)))]
         [after-note-update (update-notes (first after-burn)
                                          (life-state state))])
    (cond
      [(symbol? after-note-update) after-note-update]
      [(and (empty? loaded-notes) (for/and ([lane-state (first after-burn)])
                                    (empty? lane-state)))
       'game-over-success]
      [else (list (fingers-state state)
                  (update-burn (second after-burn))
                  (first after-note-update)
                  (update-game-tick (game-tick state))
                  (second after-note-update))])))

;; renders the guitar with its notes
(define (render state)
  (cond
    [(symbol? state) (render-game-over-screen (equal? state 'game-over-success))]
    [(list? state) ((compose (λ (guitar) (if is-playing?
                                             (render-lifebar (life-state state) guitar)
                                             guitar))
                             (λ (guitar) (render-notes (notes-state state) guitar))
                             (λ (guitar) (render-burn (burn-state state) guitar))
                             (λ (guitar) (render-fingers (fingers-state state) guitar)))
                    guitar)]))

;; maps the keyboard keys to the guitar lanes
(define finger-keys
  (hash
   "a" 0
   "s" 1
   "j" 2
   "k" 3
   "l" 4))

;; hanldes a key press/release to change the state of a finger in a lane to pressed/released
(define (handle-press pressing)
  (λ (state a-key)
    (if (and is-playing?
             (hash-has-key? finger-keys a-key))
        (let* ([lane (hash-ref finger-keys a-key)]
               [after-burn (change-burn (fingers-state state)
                                        (burn-state state)
                                        (notes-state state)
                                        (life-state state)
                                        lane
                                        pressing)])
          (if (symbol? after-burn)
              after-burn
              (list (change-fingers (fingers-state state) lane pressing)
                    (first after-burn)
                    (second after-burn)
                    (game-tick state)
                    (third after-burn))))
        state)))

(define (create-world offset mode)
  (begin (set! is-playing? (equal? mode 'play))
         (big-bang (make-world-0 offset)
           (on-tick update)
           (to-draw render)
           (on-key (handle-press #t))
           (on-release (handle-press #f))
           (stop-when symbol? render)
           (name "Racket Hero"))))