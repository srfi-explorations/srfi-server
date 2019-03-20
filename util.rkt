#lang racket

(require)

(define (must-env envar)
  (let ((val (getenv envar)))
    (if (and val (not (= 0 (string-length val))))
        val
        (error (string-append envar " environment variable not set")))))

(provide must-env)
