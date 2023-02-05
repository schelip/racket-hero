#lang racket/gui
(require "graphics.rkt")

(provide (all-defined-out))

(define offset 0)
(define resolution 192)

(define (load-notes file-name)
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
            (string->number (fourth note-info))))))

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
  (+ game-tick (clock-tick->game-tick ((compose (lambda (any) (get-tempo game-tick)) pretty-print) (get-tempo game-tick)))))

(define (get-burn-tick-offset game-tick)
  (* (clock-tick->game-tick (get-tempo game-tick))
     (/ (- guitar-height fingers-vertical-offset) 3)))

(define (spawn-note notes-state loaded-note)
  (let ([lane (second loaded-note)])
    (list-set notes-state lane
              (cons 0 (list-ref notes-state lane)))))

(define (spawn-notes game-tick notes-state loaded-notes)
  (cond
    [(empty? loaded-notes) (list notes-state loaded-notes)]
    [(< ((compose (lambda (any) game-tick) pretty-print) game-tick) (first (first loaded-notes)))  (list notes-state loaded-notes)]
    [else (spawn-notes game-tick
                       (spawn-note notes-state (first loaded-notes))
                       (rest loaded-notes))]))