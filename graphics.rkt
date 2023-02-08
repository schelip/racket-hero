#lang racket/gui
(require 2htdp/image lang/posn)

;; The objective of this module is to provide the graphics for the game. In order to do that, it must
;; be able to render the guitar, the fingers, simple and long notes, and play the animations for the
;; approaching notes and the pressing of the fingers

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------------------
;; SPRITES
;; ---------------------------------------------------------------------------------------------------

;; Color pallete for the graphics, used in adition to the default colors
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

;; Mapping of the 5 guitar lane to their colors. the lane indexes will also used to refer to
;; the finger and notes in that lane
(define finger-colors
  (hash
   0 (hash-ref colors 'green)
   1 (hash-ref colors 'red)
   2 (hash-ref colors 'yellow)
   3 (hash-ref colors 'cyan)
   4 (hash-ref colors 'orange)))

;; Finger size, which will be the base for calculating the dimensions
(define finger-size 70)

;; Finger dimensions all based on the finger-size (width)
(define finger-width finger-size)
(define finger-height (* 0.4 finger-width))
(define finger-center-width (* 0.9 finger-width))
(define finger-center-height (* 0.78 finger-height))
(define finger-pressed-width (* 0.75 finger-center-width))
(define finger-pressed-height (* 0.75 finger-center-height))

;; number -> image
;; An ellipse with a border, with the specific color for its lane
(define (make-finger lane)
  (overlay (ellipse finger-center-width finger-center-height 'solid 'black)
           (ellipse finger-width finger-height 'solid (hash-ref finger-colors lane))))

;; Ready for render finger sprites
(define fingers
  (hash
   0 (make-finger 0)
   1 (make-finger 1)
   2 (make-finger 2)
   3 (make-finger 3)
   4 (make-finger 4)))

;; number -> image
;; The finger sprite with a smaller, gray ellipse in the center
;; of the itself to indicate it as pressed
(define (press-finger lane)
  (overlay (ellipse finger-pressed-width finger-pressed-height 'solid 'gray)
           (hash-ref fingers lane)))

;; Ready for render pressed-finger sprites
(define pressed-fingers
  (hash
   0 (press-finger 0)
   1 (press-finger 1)
   2 (press-finger 2)
   3 (press-finger 3)
   4 (press-finger 4)))

;; number -> number
;; Calculates the horizontal position of the finger on a lane
(define (get-finger-horizontal-offset lane)
  (- 0 (+ fingers-horizontal-offset (* lane finger-width))))

;; number boolean scene -> scene
;; Places a finger sprite on its lane of the guitar
(define (place-finger lane pressed guitar)
  (overlay/align/offset
   'left 'bottom
   (if pressed
       (hash-ref pressed-fingers lane)
       (hash-ref fingers lane))
   (get-finger-horizontal-offset lane)
   fingers-vertical-offset
   guitar))

;; note constants, also based on the finger-size
(define note-width (* 0.93 finger-center-width))
(define note-height (* 0.4 note-width))
(define note-color-width (* 0.955 note-width))
(define note-color-height (* 0.833 note-height))
(define note-opening-border-width (* 0.444 note-width))
(define note-opening-border-height (* 0.444 note-height))
(define note-opening-width (* 0.422 note-width))
(define note-opening-height (* 0.388 note-height))

;; number -> image
;; An overlay of ellipses to create the perspective of a half-sphere with an opening in the top,
;; with the same color as the finger on its lane
(define (make-note lane)
  (overlay/align 'center 'top
                 (ellipse note-opening-width note-opening-height 'solid 'white)
                 (ellipse note-opening-border-width note-opening-border-height 'solid 'black)
                 (ellipse note-color-width note-color-height 'solid (hash-ref finger-colors lane))
                 (ellipse note-width note-height 'solid 'white)))

;; Ready for render note sprites
(define notes
  (hash
   0 (make-note 0)
   1 (make-note 1)
   2 (make-note 2)
   3 (make-note 3)
   4 (make-note 4)))

;; number number scene -> scene
;; Places an approaching note on the adequate coordinates on the guitar. It is shifted and scaled
;; so as to give the perspective effect
(define (place-note lane note-state guitar)
  (place-image (scale (get-current-scale note-state) (hash-ref notes lane))
               (get-current-x lane note-state) note-state guitar))

;; Guitar constants, also based on figer-size
(define guitar-larger-width-factor 5.5)
(define guitar-smaller-width-factor 3.5)
(define guitar-larger-width (* guitar-larger-width-factor finger-size))
(define guitar-smaller-width (/ guitar-larger-width guitar-smaller-width-factor))
(define guitar-outer-width (/ (- guitar-larger-width guitar-smaller-width) 2))
(define guitar-height (* 1.3 guitar-larger-width))
(define fingers-vertical-offset (* 0.85 finger-width))
(define fingers-horizontal-offset
  (* 0.25 (+ finger-width (/ 2 finger-height))))

;; Two triangles mirrored wrapping a rectangle to create the perspective of a tilted guitar arm
(define base-guitar
  (beside (flip-horizontal (right-triangle guitar-outer-width guitar-height 'solid 'darkgray))
          (rectangle guitar-smaller-width guitar-height 'solid 'darkgray)
          (right-triangle guitar-outer-width guitar-height 'solid 'darkgray)))

;; The pen used to draw the guitar border
(define guitar-border-pen
  (make-pen 'black 4 'solid 'butt 'bevel))

;; [number] [scene] -> scene
;; Uses basic recursion to iterate trough the five lanes and add borders and the finger to each lane
(define (make-guitar [it 0] [image base-guitar])
  (cond
    ;; First, places borders on each side of the base shape
    [(zero? it)
     (make-guitar 1 (add-line
                     (add-line image guitar-outer-width 0 0 guitar-height guitar-border-pen)
                     (- guitar-larger-width guitar-outer-width)
                     0 guitar-larger-width guitar-height
                     guitar-border-pen))]
    ;; Then, for each lane, add the finger and a border separating it from the next lane
    [(< it 6)
     (make-guitar
      (add1 it)
      (place-finger (sub1 it) #f
                    (if (= it 5) image
                        (add-line image
                                  (+ guitar-outer-width (* it (/ guitar-smaller-width 5))) 0
                                  (* it (/ guitar-larger-width 5)) guitar-height
                                  'black))))]
    [else image]))

;; The base guitar with no notes and no fingers - in other words, the background
(define guitar (make-guitar))

;; color -> image
;; Makes a flame-shaped polygon of the given color
(define (make-base-flame color)
  (scale (/ (* 0.80 finger-size) 260)
         (polygon (list (make-posn 65 310)
                        (make-posn 170 310)
                        (make-posn 230 280)
                        (make-posn 260 175)
                        (make-posn 230 75)
                        (make-posn 175 165)
                        (make-posn 125 0)
                        (make-posn 90 130)
                        (make-posn 50 80)
                        (make-posn 0 160)
                        (make-posn 30 280))
                  'solid
                  color)))

;; Flame sprite, made by overlaying a mirrored, brighter flame-polygon on another flame polygon
(define flame
  (overlay/offset (scale 0.55 (flip-horizontal (make-base-flame (hash-ref colors 'yellow))))
                  0 (- 0 (* 0.2 finger-size))
                  (make-base-flame (hash-ref colors 'orange))))

;; number -> scene
;; Places a flame in a specific lane, right above the finger of that lane
(define (place-flame lane guitar)
  (overlay/align/offset
   'left 'bottom
   flame
   (- (get-finger-horizontal-offset lane) (* 0.1 finger-width))
   (+ fingers-vertical-offset (* 0.22 finger-height))
   guitar))

;; Lifebar constants, based on the guitar dimensions
(define lifebar-width (/ guitar-outer-width 3))
(define lifebar-height (* 3 lifebar-width))
(define lifebar-border 3)
(define max-life 100)

;; Life bar sprite, made by underlaying rectangles with different colors for each section
(define lifebar
  (overlay (underlay/align 'middle 'top
                           (rectangle lifebar-width lifebar-height 'solid 'red)
                           (rectangle lifebar-width (* (/ lifebar-height 3) 2) 'solid 'yellow)
                           (rectangle lifebar-width (/ lifebar-height 3) 'solid 'green))
           (rectangle (+ lifebar-width (* lifebar-border 2)) (+ lifebar-height (* 2 lifebar-border)) 'solid 'gray)))

;; Life bar indicator sprite, a simple rectangle
(define lifebar-indicator
  (rectangle ( + lifebar-width (* 2 lifebar-border)) lifebar-border 'solid 'orange))

;; LifeState scene -> scene
;; Places the indicator in the lifebar. It subtracts the value from the max life, since the
;; y coordinates increase downwards and the life should increase upwards
(define (place-lifebar life-state guitar)
  (place-image/align (place-image/align lifebar-indicator 0
                                        (+ (* (/ (- max-life life-state) max-life) lifebar-height) lifebar-border)
                                        'left 'center lifebar)
                     guitar-larger-width 0 'right 'top
                     guitar))

;; string -> scene
;; Game over screen, made by overlaying a panel with the provided text on the base guitar
(define (game-over-screen text)
  (overlay/align 'center 'center
                 (text/font text 18 'white #f 'system 'normal 'bold #f)
                 (rectangle 200 100 'solid 'black)
                 guitar))

;; ---------------------------------------------------------------------------------------------------
;; RENDERING AND ANIMATIONS
;; ---------------------------------------------------------------------------------------------------

;; Values for the note approaching animation, that needs to get progressively larger
;; in order to follow the perspective of the guitar
(define final-note-scale (/ guitar-larger-width-factor 5))
(define initial-note-scale (/ final-note-scale guitar-smaller-width-factor))
(define final-note-width (* final-note-scale finger-width))
(define initial-note-width (* initial-note-scale finger-width))
(define note-speed 3)

;; Helper functions to calculate the position and scale of an approaching note,
;; considering the initial and final widths and the guitar dimensions.
(define (get-initial-x lane)
  (+ (+ guitar-outer-width (/ initial-note-width 2)) (* lane initial-note-width)))
(define (get-final-x lane)
  (+ (/ final-note-width 2) (* lane final-note-width)))
(define (get-tan lane)
  (/ (- (get-initial-x lane) (get-final-x lane)) guitar-height))
(define (get-current-scale note-state)
  (+ initial-note-scale
     (* (- 1 initial-note-scale)
        (/ note-state (- guitar-height fingers-vertical-offset)))))
(define (get-current-x lane note-state)
  (- (get-initial-x lane) (* (get-tan lane) note-state)))
(define (get-adjusted-speed speed note-state)
  (/ speed (/ initial-note-scale (get-current-scale note-state))))

;; FingersState number boolean -> FingersState
;; Sets the finger in a certain lane as pressed/unpressed
(define (change-fingers fingers-state lane pressing)
  (list-set fingers-state lane pressing))

;; FingersState scene -> scene
;; Uses tail recursion to render each finger on their
;; pressed/unpressed state and appropriate lane of the guitar
(define (render-fingers fingers-state guitar)
  (cond
    [(empty? fingers-state) guitar]
    [else (render-fingers (rest fingers-state)
                          (place-finger (- 5 (length fingers-state)) (first fingers-state) guitar))]))

;; Constant values for burning the notes
(define burn-range-extra 8)
(define burn-range-min
  (- (- guitar-height fingers-vertical-offset) (+ finger-height burn-range-extra)))
(define burn-range-max
  (+ (- guitar-height fingers-vertical-offset) (+ finger-height (/ burn-range-extra 2))))
(define fire-timeout 5)

;; number -> boolean
;; Checks if a note is currently close enough to a finger so as to be burned
(define (in-burn-range? note-state)
  (and (>= note-state burn-range-min) (<= note-state burn-range-max)))

;; BurnState -> BurnState
;; Uses list comprehension to pass the time for the active flames
(define (update-burn burn-state)
  (for/list ([burn-lane burn-state])
    (if (positive? burn-lane)
        (sub1 burn-lane)
        burn-lane)))

;; FingersState BurnState NotesState LifeState number boolean -> one of:
;;                                                                  List(LifeState NotesState BurnState)
;;                                                                  symbol
;; Checks if there are any notes that will be burned because of a finger press; If so, activates the
;; flame and burns those notes. Then, it updates life accordingly, considering if it was a hit (the 
;; user pressed a finger and it burned a note) or a miss (the user pressed a finger when no note was
;; on it). If it was a miss and the user was at minimum life, its game over.
(define (change-burn fingers-state burn-state notes-state life-state lane pressing)
  (let* ([new-burn-state (cond [(not pressing) burn-state]
                               [(not (for/or ([note-state (list-ref notes-state lane)]
                                              #:when (not (list-ref fingers-state lane)))
                                       (in-burn-range? note-state)))
                                burn-state]
                               [else (list-set burn-state lane fire-timeout)])]
         [new-notes-state (burn-notes new-burn-state notes-state)]
         [new-life-state (if pressing
                             (update-life notes-state new-notes-state life-state
                                          (λ (life diff) (if (zero? diff) (sub1 life) (- life diff))))
                             life-state)])
    (if (symbol? new-life-state)
        new-life-state
        (list new-burn-state
              new-notes-state
              new-life-state))))

;; BurnState NotesState -> List(NotesState BurnState)
;; Like change-burn, but burns all notes independently if the user was pressing the correct fingers
;; or not. Used for watch-only mode, so it doesn't update the lifebar.
(define (burn-all-notes burn-state notes-state)
  (let ([new-burn-state (for/list ([lane-burn-state burn-state]
                                   [lane (in-naturals)])
                          (if (for/or ([note-state (list-ref notes-state lane)])
                                (in-burn-range? note-state))
                              fire-timeout
                              lane-burn-state))])
    (list (burn-notes new-burn-state notes-state)
          new-burn-state)))

;; BurnState NotesState -> NotesState
;; Uses list comprehension to remove the note that is on the burn range for each lane, if they
;; have an active flame
(define (burn-notes burn-state notes-state)
  (for/list ([lane-state notes-state]
             [lane (in-naturals)])
    (remove (findf (λ (note-state)
                     (and (in-burn-range? note-state)
                          (positive? (list-ref burn-state lane))))
                   lane-state)
            lane-state)))

;; BurnState scene -> scene
;; Renders the active flames
(define (render-burn burn-state guitar)
  (cond
    [(empty? burn-state) guitar]
    [else (render-burn (rest burn-state)
                       (if (positive? (first burn-state))
                           (place-flame (- 5 (length burn-state)) guitar)
                           guitar))]))

;; NotesState LifeState -> List(NotesState LifeState)
;; Recalculates the approaching notes state on their lanes (because of passing time). The notes that
;; weren't burned will be counted and the LifeState will be decreased accordingly
(define (update-notes notes-state life-state)
  (let* ([new-notes-state
          (for/list ([lane-state notes-state])
            (if (empty? lane-state)
                lane-state
                (for/list ([note-state lane-state]
                           #:when (<= (+ note-state 1) guitar-height))
                  (+ note-state (get-adjusted-speed note-speed note-state)))))]
         [new-life-state (update-life notes-state new-notes-state life-state +)])
    (if (symbol? new-life-state)
        new-life-state
        (list new-notes-state
              new-life-state))))

;; NotesState scene [number] -> scene
;; Uses recursion to render the approaching notes on their lanes of the guitar
(define (render-notes notes-state guitar [lane 0])
  (cond
    [(empty? notes-state) guitar]
    [(empty? (first notes-state))
     (render-notes (rest notes-state)
                   guitar
                   (add1 lane))]
    [else (render-notes
           (cons (rest (first notes-state)) (rest notes-state))
           (place-note lane (first (first notes-state)) guitar)
           lane)]))

;; NotesState NotesState LifeState procedure -> LifeState
;; Higher order function that updates the life state base on how many notes were burned or missed.
;; First, it counts how many notes more there were in the old NotesState, then it applies the
;; procedure having the LifeState and the calculate difference as arguments.
(define (update-life old-notes-state new-notes-state life-state proc)
  (define (notes-foldr notes-state)
    (foldr + 0 (for/list ([lane-state notes-state])
                 (length lane-state))))
  (let ([new-life (proc life-state (- (notes-foldr new-notes-state) (notes-foldr old-notes-state)))])
    (if (<= new-life 0)
        'game-over-fail
        (min max-life new-life))))

;; LifeState scene -> scene
;; Renders the lifebar next to the guitar
(define (render-lifebar life-state guitar)
    (place-lifebar life-state guitar))

;; boolean -> scene
;; Renders either success or failure game over screen
(define (render-game-over-screen success)
  (game-over-screen (if success "You Rock!" "Game Over")))