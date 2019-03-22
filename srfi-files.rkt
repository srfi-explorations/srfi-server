#lang racket

(require
 file/unzip
 racket/hash
 racket/match
 racket/pretty
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

(define (gather-srfi-files-from-zip-port zip-input-port)
  (gather-srfi-files
   (lambda (handle-entry)
     (unzip
      zip-input-port
      (lambda (name-bytes dir? contents-port . _)
        (unless dir?
          (handle-entry (bytes->string/utf-8 name-bytes)
                        (lambda () (port->bytes contents-port)))))))))

(define (mcons-tree->immutable tree)
  (cond ((mpair? tree)
         (cons (mcons-tree->immutable (mcar tree))
               (mcons-tree->immutable (mcdr tree))))
        ((pair? tree)
         (cons (mcons-tree->immutable (car tree))
               (mcons-tree->immutable (cdr tree))))
        (else tree)))

(define (display-lists lists out)
  (for-each (lambda (list)
              (let ((first? #t))
                (for-each (lambda (item)
                            (unless first? (newline out))
                            (display (if first? "(" " ") out)
                            (display item out)
                            (set! first? #f))
                          list)
                (unless (null? list)
                  (display ")" out))
                (newline out)
                (newline out)))
            lists))

(define (sexp-bytes lists)
  (string->bytes/utf-8
   (call-with-output-string
    (lambda (out)
      (display-lists lists out)))))

(define (derive-args-from-html* html-bytes)
  (mcons-tree->immutable
   (call-with-input-string (bytes->string/utf-8 html-bytes)
                           process-html-port)))

(define (derive-args-from-html html-bytes)
  (sexp-bytes (derive-args-from-html* html-bytes)))

(define (derive-srfi-files-1 srfi-files-1)
  (let ((derived-files-1 (hash-copy srfi-files-1)))
    (hash-map srfi-files-1
              (lambda (srfi-suffix contents)
                (when (equal? ".html" srfi-suffix)
                  (hash-set! derived-files-1 "-args.scm"
                             (derive-args-from-html contents)))))
    derived-files-1))

(define (derive-srfi-files srfi-files)
  (let ((derived-files (make-hash)))
    (hash-for-each srfi-files
                   (lambda (srfi-number srfi-files-1)
                     (hash-set! derived-files srfi-number
                                (derive-srfi-files-1 srfi-files-1))))
    derived-files))

(define (display-srfi-files srfi-files)
  (for-each
   (lambda (srfi-number)
     (hash-for-each
      (hash-ref srfi-files srfi-number)
      (lambda (srfi-suffix contents)
        (for-each display (list srfi-number " " srfi-suffix))
        (newline))))
   (sort (hash-keys srfi-files) <)))

(define (display-srfi-files-from-zip-port zip-input-port)
  (display-srfi-files (gather-srfi-files-from-zip-port zip-input-port)))

(define (display-srfi-files-from-zip-file filename)
  (call-with-input-file filename display-srfi-files-from-zip-port))

(provide
 display-srfi-files
 derive-srfi-files
 derive-args-from-html*
 gather-srfi-files-from-zip-port
 display-srfi-files-from-zip-port
 display-srfi-files-from-zip-file)
