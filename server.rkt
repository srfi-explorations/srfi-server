#lang racket

(require
 db/base
 db/postgresql
 file/unzip
 json
 net/url
 racket/match
 racket/string
 web-server/servlet
 web-server/servlet-env)

;;;

(define web-port (if (getenv "PORT")
                     (string->number (getenv "PORT"))
                     (error "PORT environment variable not set")))

(define database-url (if (getenv "DATABASE_URL")
                         (string->url (getenv "DATABASE_URL"))
                         (error "DATABASE_URL environment variable not set")))

;;;

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

(define (database-initialize)
  (query-exec database-connection
              (string-append "create table if not exists srfi ("
                             "  srfinumber integer not null,"
                             "  filename text not null,"
                             "  contents text not null,"
                             "  primary key (srfinumber, filename)"
                             ");")))

;;;

(define (web-html-response code message headers body)
  (response/full code
                 (string->bytes/utf-8 message)
                 (current-seconds)
                 (string->bytes/utf-8 "text-html; charset=utf-8")
                 headers
                 (list (string->bytes/utf-8 body))))

(define (web-error-response status-code status-text)
  (response/full status-code
                 (string->bytes/utf-8 status-text)
                 (current-seconds)
                 (string->bytes/utf-8 "text/plain; charset=utf-8")
                 empty
                 (list (string->bytes/utf-8 status-text))))

(define (web-not-found req)
  (web-error-response 404 "Not Found"))

(define (web-method-not-allowed req)
  (web-error-response 405 "Method Not Allowed"))

(define (web-admin-github req)
  (display (bytes->jsexpr (request-post-data/raw req)) (current-error-port))
  (display #\newline (current-error-port))
  (response/xexpr '(html (body (h1 "OK")))))

(define (web-main-page req)
  (response/xexpr
   '(html (head (title "Racket Heroku App"))
          (body (h1 "It works!")))))

(define-values (web-dispatch web-url)
  (dispatch-rules
   [("")
    web-main-page]
   [("admin" "github")
    #:method "post"
    web-admin-github]
   [else
    web-not-found]))

(define (web-serve)
  (serve/servlet web-dispatch
                 #:port web-port
                 #:listen-ip #f
                 #:command-line? #t
                 #:servlet-path "/"
                 #:servlet-regexp #rx""))

(database-initialize)
(web-serve)
