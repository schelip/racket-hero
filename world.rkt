#lang racket/gui
(require 2htdp/universe
         "graphics.rkt")

;; This module controls the state of the world, rendering the appropriate graphics in the
;; correct positions based on the passing of time (song playing) and the user events

;; ---------------------------------------------------------------------------------------------------
;; WORLD
;; ---------------------------------------------------------------------------------------------------

;; initial state of the world
(define WORLD0 'resting)

(define WORLD_STATE
  (list (list #f #f #f #f #f) ;; pressed fingers
        (list 0 0 0 0 0) ;; burning animations
        (list 0 10) ;; notes in first lane
        (list 1 10 30) ;; notes in second lane ...
        (list 2 60)
        (list 3 90)
        (list 4 120)))


;; getters for each part of the world state
(define (fingers-state state)
  (first state))
(define (burn-state state)
  (second state))
(define (notes-state state)
  (rest (rest state)))

;; checks if there are any note approaching
(define (receive state message)
  (cond
    [(symbol=? state 'resting) 'running]
    [(symbol=? state 'running) WORLD_STATE]
    [else state]))

;; moves the notes every clock tick
(define (update state)
  (cond
    [(symbol? state)
     (cond
       [(symbol=? state 'running) (make-package 'running WORLD_STATE)])]
    [(list? state) (append (list (fingers-state state)
                                 (update-burn (burn-state state)))
                           (update-notes (notes-state state)
                                         (burn-state state)))]))

;; renders the guitar with its notes
(define (render state)
  (cond
    [(symbol? state) guitar]
    [(list? state) ((compose (lambda (guitar) (render-notes (notes-state state) guitar))
                             (lambda (guitar) (render-burn (burn-state state) guitar))
                             (lambda (guitar) (render-fingers (fingers-state state) guitar)))
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
        (let ([lane (hash-ref finger-keys a-key)])
          (append (list (change-fingers (fingers-state state) lane pressing)
                        (change-burn (burn-state state)
                                     (notes-state state)
                                     lane
                                     pressing))
                  (notes-state state)))
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

(create-world)