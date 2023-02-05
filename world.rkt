#lang racket/gui
(require 2htdp/universe
         "graphics.rkt"
         "chart.rkt")

;; This module controls the state of the world, rendering the appropriate graphics in the
;; correct positions based on the passing of time (song playing) and the user events

;; ---------------------------------------------------------------------------------------------------
;; WORLD
;; ---------------------------------------------------------------------------------------------------

;; initial state of the world
(define WORLD0 'resting)

(define chart-name "songs/2/notes.chart")
(define song-name "songs/2/song.mp3")
(load-sync-track chart-name)
(load-notes chart-name)


(define WORLD_STATE
  (list (list #f #f #f #f #f) ;; pressed fingers
        (list 0 0 0 0 0) ;; burning animations
        (list empty ;; notes in each lane
              empty
              empty
              empty
              empty)
        (get-clock-offset)))


;; getters for each part of the world state
(define (fingers-state state)
  (first state))
(define (burn-state state)
  (second state))
(define (notes-state state)
  (third state))
(define (game-tick state)
  (fourth state))

;; checks if there are any note approaching
(define (receive state message)
  (if (symbol? state)
      (cond [(symbol=? state 'resting) 'running]
            [(symbol=? state 'running) WORLD_STATE])
      state))

;; moves the notes every clock tick
(define (update state)
  (cond
    [(symbol? state)
     (cond
       [(symbol=? state 'running) (make-package 'running WORLD_STATE)])]
    [(list? state) (let ([spawned-notes (spawn-notes
                                         (game-tick state)
                                         (notes-state state))])
                     (list (fingers-state state)
                           (update-burn (burn-state state))
                           (update-notes spawned-notes
                                         (burn-state state))
                           (update-game-tick (game-tick state))))]))

;; renders the guitar with its notes
(define (render state)
  (cond
    [(symbol? state) guitar]
    [(list? state) ((compose (λ (guitar) (render-notes (notes-state state) guitar))
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
(define (handle-press-release pressing)
  (λ (state a-key)
    (if (hash-has-key? finger-keys a-key)
        (let* ([lane (hash-ref finger-keys a-key)]
               [after-burn (change-burn (fingers-state state)
                                        (burn-state state)
                                        (notes-state state)
                                        lane
                                        pressing)])
          (list (change-fingers (fingers-state state) lane pressing)
                (first after-burn)
                (second after-burn)
                (game-tick state)))
        state)))

(define (create-world)
  (big-bang WORLD0
    (on-receive receive)
    (on-tick update)
    (to-draw render)
    (on-key (handle-press-release #t))
    (on-release (handle-press-release #f))
    (name "guitar")
    (register LOCALHOST)))

(play-sound song-name #t)
(create-world)