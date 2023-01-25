#lang racket/gui
(require 2htdp/image 2htdp/universe)

(define current-string 2)

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

(define finger-size 70)

(define finger-width finger-size)
(define finger-height (* 0.4 finger-width))
(define finger-center-width (* 0.9 finger-width))
(define finger-center-height (* 0.78 finger-height))
(define finger-pressed-width (* 0.75 finger-center-width))
(define finger-pressed-height (* 0.75 finger-center-height))

(define finger-colors
  (hash
   0 (hash-ref colors 'green)
   1 (hash-ref colors 'red)
   2 (hash-ref colors 'yellow)
   3 (hash-ref colors 'cyan)
   4 (hash-ref colors 'orange)))

(define (make-finger color)
  (overlay (ellipse finger-center-width finger-center-height 'solid 'black)
           (ellipse finger-width finger-height 'solid (hash-ref finger-colors color))))

(define finger
  (make-finger current-string))

(define (press-finger finger)
  (overlay (ellipse finger-pressed-width finger-pressed-height 'solid 'darkgray)
           finger))

(define finger-pressed (press-finger finger))

(define note-width (* 0.93 finger-center-width))
(define note-height (* 0.4 note-width))
(define note-color-width (* 0.955 note-width))
(define note-color-height (* 0.833 note-height))
(define note-opening-border-width (* 0.444 note-width))
(define note-opening-border-height (* 0.444 note-height))
(define note-opening-width (* 0.422 note-width))
(define note-opening-height (* 0.388 note-height))

(struct note (image x y)
  #:mutable)

(define (make-note-image string)
  (overlay/align 'center 'top
                 (ellipse note-opening-width note-opening-height 'solid 'white)
                 (ellipse note-opening-border-width note-opening-border-height 'solid 'black)
                 (ellipse note-color-width note-color-height 'solid (hash-ref finger-colors string))
                 (ellipse note-width note-height 'solid 'white)))

(define (make-note string)
  (note
   (make-note-image string)
   (get-initial-x string)
   0))

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

(define base-guitar
  (beside (flip-horizontal (right-triangle guitar-outer-width guitar-height 'solid 'darkgray))
          (rectangle guitar-smaller-width guitar-height 'solid 'darkgray)
          (right-triangle guitar-outer-width guitar-height 'solid 'darkgray)))

(define guitar-border-pen
  (make-pen 'black 4 'solid 'butt 'bevel))


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

(define guitar (make-guitar))

(define final-scale (/ guitar-larger-width-factor 5))
(define initial-scale (/ final-scale guitar-smaller-width-factor))
(define final-finger-width (* final-scale finger-width))
(define initial-finger-width (* initial-scale finger-width))

(define test-note
  (make-note current-string))

(define (move-note current-state)
  (if (< current-state guitar-height)
      (+ current-state 6)
      (- current-state guitar-height)))

(define (get-initial-x string)
  (+ (+ guitar-outer-width (/ initial-finger-width 2)) (* string initial-finger-width)))
(define (get-final-x string)
  (+ (/ final-finger-width 2) (* string final-finger-width)))
(define (get-tan string)
  (/ (- (get-initial-x string) (get-final-x string)) guitar-height))
(define (get-current-x string current-state)
  (- (get-initial-x string) (* (get-tan string) current-state)))

(define (note-approaching current-state)
  (place-image (scale (+ initial-scale (* (- 1 initial-scale) (/ current-state (- guitar-height fingers-vertical-offset)))) (make-note-image current-string))
               (get-current-x current-string current-state) current-state guitar))

(big-bang 0
  (on-tick move-note)
  (to-draw note-approaching))