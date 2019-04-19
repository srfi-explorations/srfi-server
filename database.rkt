#lang racket

(require
 database-url
 db/base
 db/postgresql
 json
 net/base64
 net/url
 racket/match
 racket/set
 racket/string
 "util.rkt")

(define database-connection
  (virtual-connection (connection-pool (database-url-connector #f))))

(define (database-initialize)
  (query-exec database-connection
              (string-append "create table if not exists srfi ("
                             "  srfi_number integer not null,"
                             "  srfi_suffix text not null,"
                             "  contents text not null,"
                             "  primary key (srfi_number, srfi_suffix)"
                             ")")))

(define (database-get-srfi-table)
  (let ((srfi-table (make-hash)))
    (for-each
     (lambda (row)
       (match-let (((vector srfi-number srfi-suffix contents) row))
         (hash-set! srfi-table srfi-number
                    (hash-ref srfi-table srfi-number (make-hash)))
         (hash-set! (hash-ref srfi-table srfi-number)
                    srfi-suffix
                    (base64-decode (string->bytes/utf-8 contents)))))
     (query-rows database-connection
                 "select srfi_number, srfi_suffix, contents from srfi"))
    srfi-table))

(define (database-get-srfi-file srfi-number srfi-suffix)
  (let ((contents
         (query-maybe-value
          database-connection
          (string-append "select contents from srfi"
                         " where srfi_number = $1 and srfi_suffix = $2")
          srfi-number
          srfi-suffix)))
    (if contents
        (base64-decode (string->bytes/utf-8 contents))
        #f)))

(define (database-set-srfi-files! srfi-files)
  (call-with-transaction
   database-connection
   (lambda ()
     (hash-for-each
      srfi-files
      (lambda (srfi-number srfi-files-1)
        (hash-for-each
         srfi-files-1
         (lambda (srfi-suffix contents)
           (query-exec
            database-connection
            (string-append
             "insert"
             " into srfi (srfi_number, srfi_suffix, contents)"
             " values ($1, $2, '')"
             " on conflict (srfi_number, srfi_suffix) do nothing")
            srfi-number
            srfi-suffix)
           (query-exec
            database-connection
            (string-append
             "update srfi set contents = $3"
             " where srfi_number = $1 and srfi_suffix = $2")
            srfi-number
            srfi-suffix
            (bytes->string/utf-8 (base64-encode contents ""))))))))))

(provide
 database-initialize
 database-get-srfi-table
 database-get-srfi-file
 database-set-srfi-files!)
