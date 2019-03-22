#! /usr/bin/env racket

#lang racket

;; SPDX-License-Identifier: MIT
;; Copyright 2019 The srfi-server authors

(require
 "srfi-files.rkt")

(define (main)
  (define verbose (make-parameter #f))
  (define filename
    (command-line
     #:multi
     [("-v" "--verbose")
      "Show detailed output"
      (verbose #t)]
     #:args (filename)
     filename))
  (hash-for-each
   (derive-srfi-files
    (call-with-input-file filename gather-srfi-files-from-zip-port))
   (lambda (srfi-number srfi-files-1)
     (display (hash-ref srfi-files-1 "-args.scm")))))

(main)
