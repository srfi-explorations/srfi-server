#lang racket

(require
 db/base
 db/postgresql
 json
 net/base64
 net/url
 racket/match
 racket/set
 racket/string
 "util.rkt")

(define database-url
  (string->url (must-env "DATABASE_URL")))

(match-define
  (url _
       database-username/password
       database-host
       database-port
       _
       (list (path/param database _)) _ _)
  database-url)

(define-values (database-username database-password)
  (match (string-split database-username/password ":")
    ((list username)          (values username #f))
    ((list username password) (values username password))))

(define (database-connect)
  (postgresql-connect #:user database-username
                      #:password database-password
                      #:database database
                      #:server database-host
                      #:port database-port))

(define database-connection
  (virtual-connection (connection-pool database-connect)))

(define (database-initialize database-url)
  (query-exec database-connection
              (string-append "create table if not exists srfi ("
                             "  srfi_number integer not null,"
                             "  srfi_suffix text not null,"
                             "  contents text not null,"
                             "  primary key (srfi_number, srfi_suffix)"
                             ")")))

(define (database-get-srfi-table)
  (let ((srfi-table (make-hash)))
    (call-with-transaction
     database-connection
     (lambda ()
       (for-each
        (lambda (row)
          (match-let (((vector srfi-number srfi-suffix contents) row))
            (hash-set! srfi-table srfi-number
                       (hash-ref srfi-table srfi-number (make-hash)))
            (hash-set! (hash-ref srfi-table srfi-number)
                       srfi-suffix
                       (base64-decode (string->bytes/utf-8 contents)))))
        (query-rows database-connection
                    "select srfi_number, srfi_suffix, contents from srfi"))))
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

(define (database-set-srfi-file! srfi-number srfi-suffix contents)
  (query-exec database-connection
              (string-append
               "insert"
               " into srfi (srfi_number, srfi_suffix, contents)"
               " values ($1, $2, '')"
               " on conflict (srfi_number, srfi_suffix) do nothing")
              srfi-number
              srfi-suffix)
  (query-exec database-connection
              (string-append "update srfi set contents = $3"
                             " where srfi_number = $1 and srfi_suffix = $2")
              srfi-number
              srfi-suffix
              (bytes->string/utf-8 (base64-encode contents ""))))

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
           (database-set-srfi-file! srfi-number srfi-suffix contents))))))))

(provide
 database-initialize
 database-get-srfi-table
 database-get-srfi-file
 database-set-srfi-files!)
