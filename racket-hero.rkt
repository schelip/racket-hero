;; Racket Hero
;;
;; Desenvolvido durante a disciplina de PPLF (6902/02)
;; no terceiro ano de Ciência da Computação na UEM
;; para realização do Trbalho Prático 1
;; 
;; Alunos:
;;  Felipe Gabriel Comin Scheffel - RA117306
;;  Douglas Kenji Sakakibara - RA117741
;;
;; Professor: Wagner Igarashi

#lang racket/gui
(require "chart.rkt" "world.rkt")

(provide is-playing?)

(define play-mode (make-parameter 'play))
(define (is-playing?) (equal? (play-mode) 'play))

(define mode (make-parameter 'play))
(define song-option (make-parameter null))
(define chart-file (make-parameter "notes.chart"))
(define audio-file (make-parameter null))

(define (start-game)
  (begin
    (load-chart
     (string-append "songs/" (song-option) "/" (chart-file)))
    (and (not (null? (audio-file)))
         (play-sound (string-append "songs/" (song-option) "/" (audio-file)) #t))
    (create-world offset (mode))))

(define (show-list)
  (for-each (λ (path) (pretty-print (path->string path)))
            (directory-list "songs")))

(command-line
 #:program "racket-hero"

 #:help-labels "Operations to perform:"

 #:once-any
 [("-p" "--play") SONG-OPTION
                  "Starts the game with the provided './songs' subdirectory"
                  (song-option SONG-OPTION)]

 [("-l" "--list") "Lists the available songs to be played"
                  (mode 'list)]

 #:once-each
 [("-c" "--chart") CHART-FILE
                   "The chart file at the './songs' subdir (default: notes.chart"
                   (chart-file CHART-FILE)]

 [("-a" "--audio") SONG-FILE
                   "(EXPERIMENTAL) The audio file at the './songs' subdir to be played along with the chart"
                   (audio-file SONG-FILE)]

 [("-w" "--watch-only") "The computer itself plays the game, without player interaction"
                        (mode 'watch)]

 #:args ()
 (if (equal? (mode) 'list)
     (show-list)
     (start-game)))