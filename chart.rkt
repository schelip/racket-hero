#lang racket/gui
(require "graphics.rkt")

(provide (all-defined-out))

(define offset 0)
(define resolution 192)

(define (get-clock-offset)
  ; (/ (- guitar-height fingers-vertical-offset)
  ;  (clock-tick->game-tick (get-tempo 0))))
  0)

(define loaded-notes null)

(define (load-notes file-name)
  (set! loaded-notes
        (let* ([lines (file->lines file-name)]
               [tail (drop lines (+ (index-of lines "[ExpertSingle]") 2))]
               [slice (take tail (sub1 (index-of tail "}")))])
          (for/list ([line slice]
                     #:do [(define note-info
                             (let ([rmatch (regexp-match*
                                            #rx"  (.+) = (.+) (.+) (.+)"
                                            line
                                            #:match-select cdr)])
                               (if (empty? rmatch)
                                   rmatch
                                   (first rmatch))))]
                     #:when (and (not (empty? note-info))
                                 (equal? (second note-info) "N")
                                 (<= (string->number (third note-info)) 4)))
            (list (string->number (first note-info))
                  (string->number (third note-info))
                  (string->number (fourth note-info)))))))

(define sync-track null)

(define (load-sync-track file-name)
  (define (make-sync-track sync-lines current-ts current-bpm)
    (cond
      [(empty? sync-lines) sync-lines]
      [(equal? (second (first sync-lines)) "B")
       (cons (list (first (first sync-lines))
                   current-ts
                   (/ (third (first sync-lines)) 1000))
             (make-sync-track (rest sync-lines)
                              current-ts
                              (/ (third (first sync-lines)) 1000)))]
      [else ;;(equal? (second (first sync-lines)) "TS")
       (cons (list (first (first sync-lines))
                   (third (first sync-lines)) current-bpm)
             (make-sync-track (rest sync-lines)
                              (third (first sync-lines))
                              current-bpm))]))
  (set! sync-track
        (let* ([lines (file->lines file-name)]
               [tail (drop lines (+ (index-of lines "[SyncTrack]") 2))]
               [slice (take tail (sub1 (index-of tail "}")))]
               [sync-lines (for/list ([line slice])
                             (let ([rmatch (regexp-match*
                                            #rx"  (.+) = (.+) (.+)"
                                            line
                                            #:match-select cdr)])
                               (if (empty? rmatch)
                                   rmatch
                                   (list (string->number (first (first rmatch)))
                                         (second (first rmatch))
                                         (string->number (third (first rmatch)))))))]
               [start-ts (third (first sync-lines))]
               [start-bpm (/ (third (second sync-lines)) 1000)])
          (cons (list 0 start-ts start-bpm) (make-sync-track (rest (rest sync-lines)) start-ts start-bpm)))))

(define (get-tempo game-tick)
  (if (> game-tick (first (last sync-track)))
      (rest (last sync-track))
      (for/first ([tempo sync-track]
                  #:when (>= (first tempo) game-tick))
        (rest tempo))))

(define (clock-tick->game-tick tempo)
  (let* ([time-signature (first tempo)]
         [bpm (second tempo)]
         [mspb (* (/ time-signature bpm) 60000)]
         [mspt (/ mspb (* resolution time-signature))])
    (/ (/ 1000 28) mspt)))

(define (update-game-tick game-tick)
  (+ game-tick (clock-tick->game-tick (get-tempo game-tick))))

(define (get-next-loaded-note)
  (let ([next-note (first loaded-notes)])
    (set! loaded-notes (rest loaded-notes))
    next-note))

(define clock-offset (/ (- guitar-height fingers-vertical-offset) note-speed))

(define (should-spawn? game-tick)
  (>= game-tick
      (- (first (first loaded-notes))
         307)))

(define (spawn-note notes-state loaded-note)
  (let ([lane (second loaded-note)])
    (list-set notes-state lane
              (cons 0 (list-ref notes-state lane)))))

(define (spawn-notes game-tick notes-state)
  (cond
    [(empty? loaded-notes) notes-state]
    [(should-spawn? game-tick)
     (spawn-notes game-tick
                  (spawn-note notes-state (get-next-loaded-note)))]
    [else notes-state]))