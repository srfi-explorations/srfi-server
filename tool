#! /usr/bin/env racket

#lang racket

;; SPDX-License-Identifier: MIT
;; Copyright 2019 The srfi-server authors

(require
 file/unzip
 racket/match
 rackunit
 "srfi-tool/srfi-tool.rkt")

(define (split-srfi-path path)
  (let* ((dir (let ((x (path-only path)))
                (if x (some-system-path->string x) #f)))
         (base (let ((x (file-name-from-path path)))
                 (if x (some-system-path->string x) ""))))
    (match (regexp-match #rx"^srfi-(0|[1-9][0-9]*)([-.].*)$" base)
      ((list _ srfi-number srfi-suffix)
       (list dir (string->number srfi-number) srfi-suffix))
      (else
       (list #f #f #f)))))

(test-case "split-srfi-path"
  (check-equal? (split-srfi-path "srfi-1.html")
                '(#f 1 ".html"))
  (check-equal? (split-srfi-path "srfi-1-args.scm")
                '(#f 1 "-args.scm"))
  (check-equal? (split-srfi-path "srfi-1-info.scm")
                '(#f 1 "-info.scm"))
  (check-equal? (split-srfi-path "srfi-1-info-add.scm")
                '(#f 1 "-info-add.scm"))
  (check-equal? (split-srfi-path "srfi-x.html")
                '(#f #f #f))
  (check-equal? (split-srfi-path "srfi-x-1.html")
                '(#f #f #f))
  (check-equal? (split-srfi-path "srfi-1/")
                '(#f #f #f))
  (check-equal? (split-srfi-path "foo/srfi-23-info-add.scm")
                '("foo/" 23 "-info-add.scm")))

(define srfi-suffixes
  '(".html"
    "-args.scm"
    "-info.scm"
    "-info-add.scm"))

(define (gather-srfi-files walk)
  (let ((srfi-files (make-hash)))
    (walk
     (lambda (path get-contents)
       (match-let (((list dir srfi-number srfi-suffix) (split-srfi-path path)))
         (when (and srfi-number srfi-suffix (member srfi-suffix srfi-suffixes))
           (hash-set! srfi-files srfi-number
                      (hash-ref srfi-files srfi-number (make-hash)))
           (hash-set! (hash-ref srfi-files srfi-number)
                      srfi-suffix
                      (get-contents))))))
    srfi-files))

(define (gather-srfi-files-from-zip zip-input-port)
  (gather-srfi-files
   (lambda (handle-entry)
     (unzip
      zip-input-port
      (lambda (name-bytes dir? contents-port . _)
        (unless dir?
          (handle-entry (bytes->string/utf-8 name-bytes)
                        (lambda () (port->bytes contents-port)))))))))

(define (display-srfi-files srfi-files)
  (for-each (lambda (srfi-number)
              (hash-map (hash-ref srfi-files srfi-number)
                        (lambda (srfi-suffix contents)
                          (when (equal? ".html" srfi-suffix)
                            (for-each display (list srfi-number " " srfi-suffix))
                            (newline)
                            (call-with-input-bytes
                             contents
                             (lambda (port)
                               (call-with-input-string
                                (bytes->string/utf-8 (port->bytes port))
                                process-html-port)))))))
            (sort (hash-keys srfi-files) <)))

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
  (display-srfi-files (call-with-input-file filename gather-srfi-files-from-zip)))

(main)
