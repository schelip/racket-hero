#lang racket/gui
(require 2htdp/universe)

(define (add-world univ wrld)
  (local ((define univ* (append univ (list wrld))))
    (make-bundle univ*
                 (list (make-mail (first univ*) 'resting))
                 '())))

(define (switch univ wrld m)
  (local ((define univ* (append (rest univ) (list (first univ)))))
    (make-bundle univ*
                 (list (make-mail (first univ*) 'resting))
                 '())))

(universe '() (on-new add-world) (on-msg switch))