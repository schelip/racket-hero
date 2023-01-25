#lang racket/gui
(require pict racket/draw racket/include)

(define (on-game-event event)
 (void))

;; A frame which intercepts keyboard input using the `on-subwindow-char`
;; method and passes it to `on-game-event` -- this is used to read keyboard
;; input from the user and press the right note.
(define game-frame%
  (class frame%
    (init) (super-new)
    (define/override (on-subwindow-char receiver event)
      (on-game-event event)
      (super on-subwindow-char receiver event))))

;; The dimensions of the playing field
(define-values (window-width window-height)
  (values 180 360))

;; The toplevel window for the game

(define toplevel
  (new game-frame% [label "Racket Hero"] [width window-width] [height window-height]))

;; Panel which holds all the controls and sub-panels in the game
(define game-panel (new horizontal-panel% [parent toplevel] [spacing 20] [border 20]))

;; A canvas which holds the drawing area for the game -- the on-tetris-paint
;; defined above is used to fill the canvas, and will be invoked when the
;; canvas is refreshed.
(define play-field (new canvas% [parent game-panel]
                        [min-width window-width]
                        [min-height window-height]
                        [stretchable-width #f]
                        [stretchable-height #f]))

(define (start-game)
  (send play-field focus)
  (send toplevel show #t))

(start-game)