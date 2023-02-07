#lang racket/gui
(require "chart.rkt" "world.rkt")

(define play #f)

(define (list-options) (void))

(define song-option (make-parameter null))
(define chart-file (make-parameter "notes.chart"))
(define audio-file (make-parameter null))

(define (set-song-option SONG-OPTION)
  (begin (set! song-option SONG-OPTION)
         (set! play #t)))

(define (start-game song-option)
  (begin
    (load-chart
     (string-append "songs/" song-option "/" (chart-file)))
    (and (not (null? (audio-file)))
         (play-sound (string-append "songs/" song-option "/" (audio-file)) #t))
    (create-world offset)))

(define (show-list)
  (for-each (lambda (path) (pretty-print (path->string path)))
            (directory-list "songs")))

(command-line
 #:program "racket-hero"

 #:help-labels "Operations to perform:"

 #:once-any
 [("-p" "--play") SONG-OPTION
                  "Starts the game with the provided './songs' subdirectory"
                  (set-song-option SONG-OPTION)]
 [("-l" "--list") "Lists the available songs to be played"
                  (set! play #f)]

 #:once-each
 [("-c" "--chart") CHART-FILE
                   "The chart file at the './songs' subdir (default: notes.chart"
                   (chart-file CHART-FILE)]

 [("-a" "--audio") SONG-FILE
                   "(EXPERIMENTAL) The audio file at the './songs' subdir to be played along with the chart"
                   (audio-file SONG-FILE)]

 #:args ()
 (if play
     (start-game song-option)
     (show-list)))