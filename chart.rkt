#lang racket/gui

;; The objective of this module is to parse a chart file, defining enough information to spawn notes
;; in syncronization with the music.

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------------------
;; LOADING
;; ---------------------------------------------------------------------------------------------------

;; The state that will be defined after parsing the chart
(define offset 0)
(define resolution 192)
(define loaded-notes null)
(define sync-track null)

;; Wrapper to call each loading function
(define (load-chart file-name)
  (begin
    (load-song-info file-name)
    (load-notes file-name)
    (load-sync-track file-name)))

;; Uses regex to iterate the lines and parse the necessary info on the [Song] section:
;; the Offset and the Resolution
(define (load-song-info file-name)
  (define (parse-info info lines)
    (for/or ([line lines])
      (let ([rmatch (regexp-match*
                     (regexp (string-append "  " info " = (.+)"))
                     line
                     #:match-select cadr)])
        (and (not (empty? rmatch)) (string->number (first rmatch))))))
  (let* ([lines (file->lines file-name)]
         [offset-info (parse-info "Offset" lines)]
         [resolution-info (parse-info "Resolution" lines)])
    (and offset-info (set! offset offset-info))
    (and resolution-info (set! resolution resolution-info))))

;; Uses regex and list comprehension to iterate the lines and parse the notes
;; information on the [ExpertSingle] section. For now, only regular notes are considered,
;; so informations about open, HOPO, and super notes, for example, will be ignored.
;; Also, it parses information about the sustain time of the note, but for now all notes
;; are treated as if they had 0 sustain.
;; It will set a list of lists of the LoadedNote structure, List(number number number), being:
;;    first: The game tick when the note should be spawned
;;    second: The lane in which to spawn the note
;;    third: The sustain value for the note (unused)
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

;; Uses an inner function recursively to load information about the [SyncTrack] section. The inner
;; function is used because each line sets either the new timestamp or BPM of the chart, so each
;; recursive call uses the new parsed value for one information and keeps the last parsed value for
;; the other.
;; It will set a list of lists of the Tempo structure, List(number number number), being:
;;    first: The game tick when the tempo should begin to be applied
;;    second: The Time Stamp of the tempo
;;    third: The BPM of the tempo
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
          (cons (list 0 start-ts start-bpm)
                (make-sync-track (rest (rest sync-lines)) start-ts start-bpm)))))


;; ---------------------------------------------------------------------------------------------------
;; SYNCHRONIZATION
;; ---------------------------------------------------------------------------------------------------

;; GameTick -> Tempo
;; Iterates trough the sync-track info to get the timestamp and bpm information for the
;;  specified game tick
(define (get-tempo game-tick)
  (if (> game-tick (first (last sync-track)))
      (rest (last sync-track))
      (for/first ([tempo sync-track]
                  #:when (>= (first tempo) game-tick))
        (rest tempo))))

;; Tempo -> GameTick
;; Converts one clock-tick (updated 28 times by second) to the value in game-ticks,
;; that has a volatile increase frequency based on the TS and BPM.
;; The formula for the conversion was gotten from:
;; https://specterdev.ca/2018/writing-rhythm-game-engine-p3/
(define (clock-tick->game-tick tempo)
  (let* ([time-signature (first tempo)]
         [bpm (second tempo)]
         [mspb (* (/ time-signature bpm) 60000)]
         [mspt (/ mspb (* resolution time-signature))])
    (/ (/ 1000 28) mspt)))

;; GameTick -> GameTick
;; Adds the adequate value for the current tempo on the game-tick counter
(define (update-game-tick game-tick)
  (+ game-tick (clock-tick->game-tick (get-tempo game-tick))))

;; -> LoadedNote
;; Pops the first note from the laoded notes
(define (get-next-loaded-note)
  (let ([next-note (first loaded-notes)])
    (set! loaded-notes (rest loaded-notes))
    next-note))

;; GameTick -> boolean
;; Checks if the current game-tick counter is enough to spawn the next note
(define (should-spawn? game-tick)
  (>= game-tick (first (first loaded-notes))))

;; NotesState LoadedNote -> NotesState
;; Adds a loaded note to the starting state of a lane on NotesState
(define (spawn-note notes-state loaded-note)
  (let ([lane (second loaded-note)])
    (list-set notes-state lane
              (cons 0 (list-ref notes-state lane)))))

;; GameTick NotesState -> NotesState
;; Uses recursion to spawn all the pending loaded notes
(define (spawn-notes game-tick notes-state)
  (cond
    [(empty? loaded-notes) notes-state]
    [(should-spawn? game-tick)
     (spawn-notes game-tick
                  (spawn-note notes-state (get-next-loaded-note)))]
    [else notes-state]))