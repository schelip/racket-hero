#lang racket/gui
(require 2htdp/image 2htdp/universe)

;; ---------------------------------------------------------------------------------------------------
;; BASE GRAPHICS
;; ---------------------------------------------------------------------------------------------------

;; color pallete for the graphics, used in adition to the default colors
(define colors
  (hash
   'white (make-color 255 255 255)
   'green (make-color 34 136 51)
   'red (make-color 204 51 17)
   'yellow (make-color 204 187 68)
   'cyan (make-color 51 187 238)
   'orange (make-color 238 119 51)
   'gray (make-color 187 187 187)
   'darkgray (make-color 85 85 85)
   'black (make-color 0 0 0)))

;; mapping of the 5 guitar lane to their colors. the lane indexes will also used to refer to
;; the finger and notes in that lane
(define finger-colors
  (hash
   0 (hash-ref colors 'green)
   1 (hash-ref colors 'red)
   2 (hash-ref colors 'yellow)
   3 (hash-ref colors 'cyan)
   4 (hash-ref colors 'orange)))

;; finger size, which will be the base for calculating the dimensions
(define finger-size 70)

;; finger dimensions all based on the finger-size (width)
(define finger-width finger-size)
(define finger-height (* 0.4 finger-width))
(define finger-center-width (* 0.9 finger-width))
(define finger-center-height (* 0.78 finger-height))
(define finger-pressed-width (* 0.75 finger-center-width))
(define finger-pressed-height (* 0.75 finger-center-height))

;; an ellipse with a border, with a specific color for each lane
(define (make-finger lane)
  (overlay (ellipse finger-center-width finger-center-height 'solid 'black)
           (ellipse finger-width finger-height 'solid (hash-ref finger-colors lane))))

;; a smaller, gray ellipse in the center of the finger to indicate it as pressed
(define (press-finger finger)
  (overlay (ellipse finger-pressed-width finger-pressed-height 'solid 'darkgray)
           finger))

;; note constants, also based on the finger-size
(define note-width (* 0.93 finger-center-width))
(define note-height (* 0.4 note-width))
(define note-color-width (* 0.955 note-width))
(define note-color-height (* 0.833 note-height))
(define note-opening-border-width (* 0.444 note-width))
(define note-opening-border-height (* 0.444 note-height))
(define note-opening-width (* 0.422 note-width))
(define note-opening-height (* 0.388 note-height))

;; an overlay of ellipses to create the perspective of a half-sphere with an opening in the top,
;; with the same color as the finger on its lane
(define (make-note lane)
  (overlay/align 'center 'top
                 (ellipse note-opening-width note-opening-height 'solid 'white)
                 (ellipse note-opening-border-width note-opening-border-height 'solid 'black)
                 (ellipse note-color-width note-color-height 'solid (hash-ref finger-colors lane))
                 (ellipse note-width note-height 'solid 'white)))

;; guitar constants, also based on figer-size
(define guitar-separator-width (/ finger-size 10))
(define guitar-larger-width-factor 5.5)
(define guitar-smaller-width-factor 3.5)
(define guitar-larger-width (* guitar-larger-width-factor finger-size))
(define guitar-smaller-width (/ guitar-larger-width guitar-smaller-width-factor))
(define guitar-outer-width (/ (- guitar-larger-width guitar-smaller-width) 2))

(define guitar-height (* 1.3 guitar-larger-width))
(define fingers-vertical-offset (* 0.85 finger-width))
(define fingers-horizontal-offset
  (* 0.25 (+ finger-width (/ 2 finger-height))))

;; two triangles mirrored wrapping a rectangle to create the perspective of a tilted guitar arm
(define base-guitar
  (beside (flip-horizontal (right-triangle guitar-outer-width guitar-height 'solid 'darkgray))
          (rectangle guitar-smaller-width guitar-height 'solid 'darkgray)
          (right-triangle guitar-outer-width guitar-height 'solid 'darkgray)))

(define guitar-border-pen
  (make-pen 'black 4 'solid 'butt 'bevel))

;; adds borders, separates the guitar in 5 sections (the 5 lanes) and adds a finger to each lane
(define (make-guitar [it 0] [image base-guitar])
  (cond
    [(zero? it) (make-guitar 1 (add-line
                                (add-line
                                 image
                                 guitar-outer-width 0 0 guitar-height
                                 guitar-border-pen)
                                (- guitar-larger-width guitar-outer-width) 0 guitar-larger-width guitar-height
                                guitar-border-pen))]
    [(= it 6) image]
    [else (make-guitar (add1 it) (overlay/align/offset
                                  'left 'bottom
                                  (make-finger (sub1 it))
                                  (- 0 (+ fingers-horizontal-offset (* (sub1 it) finger-width))) fingers-vertical-offset
                                  (if (= it 5)
                                      image
                                      (add-line image
                                                (+ guitar-outer-width (* it (/ guitar-smaller-width 5))) 0 (* it (/ guitar-larger-width 5)) guitar-height
                                                'black))))]))

;; there will be only one guitar world
(define guitar (make-guitar))

;; ---------------------------------------------------------------------------------------------------
;; ANIMATIONS
;; ---------------------------------------------------------------------------------------------------

;; values for the note approaching animation, that needs to get progressively larger
;; in order to follow the perspective of the guitar
(define final-finger-scale (/ guitar-larger-width-factor 5))
(define initial-finger-scale (/ final-finger-scale guitar-smaller-width-factor))
(define final-finger-width (* final-finger-scale finger-width))
(define initial-finger-width (* initial-finger-scale finger-width))

;; helper functions to calculate the position of an approaching note,
;; considering the initial and final widths and the guitar dimensions
(define (get-initial-x lane)
  (+ (+ guitar-outer-width (/ initial-finger-width 2)) (* lane initial-finger-width)))
(define (get-final-x lane)
  (+ (/ final-finger-width 2) (* lane final-finger-width)))
(define (get-tan lane)
  (/ (- (get-initial-x lane) (get-final-x lane)) guitar-height))

;; calculates the scale of an approaching note so that it follows the perspective of the guitar
(define (get-current-scale state)
  (+ initial-finger-scale
     (* (- 1 initial-finger-scale)
        (/ state (- guitar-height fingers-vertical-offset)))))
;; calculates the position of an approaching note so that it follows the perspective of the guitar
(define (get-current-x lane state)
  (- (get-initial-x lane) (* (get-tan lane) state)))

;; places an approaching note
(define (note-approaching lane state guitar)
  (place-image (scale (get-current-scale state) (make-note lane))
               (get-current-x lane state) state guitar))

(define (render-notes state current-guitar)
  (cond
    [(empty? state) current-guitar]
    [(empty? (rest (first state))) (render-notes (rest state) current-guitar)]
    [else
     (let ([lane (first (first state))]
           [note-states (rest (first state))])
       (render-notes (cons (cons lane (rest note-states))
                           (rest state))
                     (note-approaching lane (first note-states) current-guitar)))]))

(define (reposition-notes-lane lane-state)
  (for/list ([note lane-state]
             #:when (<= (+ note 6) guitar-height))
    (+ note 6)))

(define (reposition-notes state)
  (for/list ([lane-state state]
             #:unless (empty? (rest lane-state)))
    (cons (first lane-state) (reposition-notes-lane (rest lane-state)))))

;; ---------------------------------------------------------------------------------------------------
;; WORLD
;; ---------------------------------------------------------------------------------------------------

;; initial state of the world
(define WORLD0 'resting)

(define test-notes
  (list (list 0 0)
        (list 1 30)
        (list 2 60)
        (list 3 90)
        (list 4 120)))

;; checks if there are any note approaching
(define (receive state message)
  (cond
    [(symbol=? state 'resting) 'running]
    [(symbol=? state 'running) test-notes]
    [else state]))

;; moves the notes every clock tick
(define (update state)
  (cond
    [(symbol? state)
     (cond
       [(symbol=? state 'running) (make-package 'running test-notes)])]
    [(list? state)
       (reposition-notes state)]))

;; renders the guitar with its notes
(define (render state)
    (cond
      [(symbol? state) guitar]
      [(list? state) (render-notes state guitar)]))

(define (create-world)
  (big-bang WORLD0
    (on-receive receive)
    (on-tick update)
    (to-draw render)
    (name "guitar")
    (register LOCALHOST)))

(create-world)