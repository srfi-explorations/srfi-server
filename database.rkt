#lang racket

(require
 db/base
 db/postgresql
 json
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
                             ");")))

(provide database-initialize)
