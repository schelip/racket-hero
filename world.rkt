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
(define WORLD0 'run)

(define chart-name "songs/1/notes.chart.full")
(define song-name "songs/1/song.mp3")
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
        (get-clock-offset)
        (/ max-life 2)))

(define game-over-timeout (* 28 3))

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

;; checks if there are any note approaching
(define (receive state message)
  (if (symbol? state)
      (cond [(symbol=? state 'run) WORLD_STATE]
            [(symbol=? state 'game-over-fail) #f]
            [(symbol=? state 'game-over-success) #t])
      state))

;; moves the notes every clock tick
(define (update state)
  (cond
    [(symbol? state)
     (cond
       [(symbol=? state 'run) (make-package 'run WORLD_STATE)])]
    [(list? state) (let* ([spawned-notes (spawn-notes
                                          (game-tick state)
                                          (notes-state state))]
                          [after-note-update (update-notes spawned-notes
                                                           (life-state state))])
                     (cond
                       [(package? after-note-update) after-note-update]
                       [(and (empty? loaded-notes) (for/and ([lane-state spawned-notes])
                                                     (empty? lane-state)))
                        (make-package 'game-over-success #t)]
                       [else (list (fingers-state state)
                                   (update-burn (burn-state state))
                                   (first after-note-update)
                                   (update-game-tick (game-tick state))
                                   (second after-note-update))]))]))

;; renders the guitar with its notes
(define (render state)
  (cond
    [(symbol? state) guitar]
    [(boolean? state) (render-game-over-screen state)]
    [(list? state) ((compose (λ (guitar) (render-lifebar (life-state state) guitar))
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
(define (handle-press-release pressing)
  (λ (state a-key)
    (if (hash-has-key? finger-keys a-key)
        (let* ([lane (hash-ref finger-keys a-key)]
               [after-burn (change-burn (fingers-state state)
                                        (burn-state state)
                                        (notes-state state)
                                        (life-state state)
                                        lane
                                        pressing)])
          (if (package? after-burn)
              after-burn
              (list (change-fingers (fingers-state state) lane pressing)
                    (first after-burn)
                    (second after-burn)
                    (game-tick state)
                    (third after-burn))))
          state)))

(define (create-world)
  (big-bang WORLD0
    (on-receive receive)
    (on-tick update)
    (to-draw render)
    (on-key (handle-press-release #t))
    (on-release (handle-press-release #f))
    (stop-when boolean? render)
    (name "Racket Hero")
    (register LOCALHOST)))

(play-sound song-name #t)
(create-world)