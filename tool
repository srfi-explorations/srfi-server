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
  (display-srfi-files-from-zip-file filename))

(main)
