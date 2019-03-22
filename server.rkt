#lang racket

(require
 css-expr
 file/unzip
 json
 net/url
 racket/match
 racket/set
 racket/string
 web-server/servlet
 web-server/servlet-env
 "util.rkt"
 "database.rkt"
 "github.rkt"
 "srfi-files.rkt")

;;;

(define web-port
  (string->number (must-env "PORT")))

(define database-url
  (string->url (must-env "DATABASE_URL")))

;;;

(define (web-text-bytes-response body)
  (response/full 200 #"OK"
                 (current-seconds)
                 (string->bytes/utf-8 "text/plain; charset=utf-8")
                 empty
                 (list body)))

(define (web-html-bytes-response body)
  (response/full 200 #"OK"
                 (current-seconds)
                 (string->bytes/utf-8 "text/html; charset=utf-8")
                 empty
                 (list body)))

;;

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

(define (web-unauthorized req)
  (web-error-response 403 "Forbidden"))

(define (repo-name->srfi-number repo-name)
  (match (regexp-match #rx"^srfi-([0-9]+)$" repo-name)
    ((list _ srfi-number) (string->number srfi-number))
    (else #f)))

(define (web-admin-github req)
  (let* ((req-bytes (request-post-data/raw req)))
    (cond ((not (github-sha1-match? req req-bytes))
           (web-unauthorized req))
          (else
           (let ((j (bytes->jsexpr req-bytes)))
             (display j (current-error-port))
             (display #\newline (current-error-port))
             (let* ((repository (hash-ref j 'repository))
                    (repo-name (hash-ref repository 'name))
                    (srfi-number (repo-name->srfi-number repo-name)))
               (fprintf (current-error-port)
                        "The repo name is <~a>~n" repo-name)
               (cond ((not srfi-number)
                      (fprintf (current-error-port)
                               "This is not a SRFI repo~n"))
                     (else
                      (fprintf (current-error-port)
                               "The repo SRFI number is <~a>~n" srfi-number)
                      (let* ((commit-hash (hash-ref j 'after))
                             (zip-url (github-zip-url repository commit-hash)))
                        (fprintf (current-error-port)
                                 "The zip URL is <~a>~n" zip-url)
                        (fprintf (current-error-port)
                                 "Reading zip...~n")
                        (call/input-url
                         (string->url zip-url)
                         (lambda (url) (get-pure-port url '() #:redirections 1))
                         (compose database-set-srfi-files!
                                  derive-srfi-files
                                  gather-srfi-files-from-zip-port)))))
               (response/xexpr '(html (body (h1 "OK"))))))))))

(define (web-main-page req)
  (response/xexpr
   `(html
     (head
      (title "SRFI Status Dashboard")
      (style
          ,(css-expr->css
            (css-expr
             [html #:font-family sans-serif]
             [table #:border-collapse collapse]
             [table td th
                    #:border (1px solid black)
                    #:padding 5px]

             [td.srfi-number #:text-align center]

             [td.srfi-status
              #:text-align center
              #:font-variant small-caps]
             [td.srfi-status.draft
              #:background-color lightyellow]
             [td.srfi-status.draft::after
              #:content "draft"]
             [td.srfi-status.final
              #:background-color lightgreen]
             [td.srfi-status.final::after
              #:content "final"]
             [td.srfi-status.withdrawn
              #:background-color lightblue]
             [td.srfi-status.withdrawn::after
              #:content "withdrawn"]

             [td.volunteer-status
              #:text-align center]
             [td.volunteer-status.ok
              #:background-color lightgreen]
             [td.volunteer-status.ok::after
              #:content "\u2714"]
             [td.volunteer-status.pending
              #:background-color lightyellow]
             [td.volunteer-status.pending::after
              #:content "\u2715"])))
      (body
       (h1 "SRFI Status Dashboard")
       (table
        (tr
         (th ((colspan "2")) "SRFI")
         (th "Status")
         (th "Markup")
         (th "Metadata")
         (th "Types"))))))))

(define (web-api-srfi-file send-response srfi-suffix req srfi-number)
  (let ((contents (database-get-srfi-file srfi-number srfi-suffix)))
    (if (not contents)
        (web-not-found req)
        (send-response contents))))

(define web-api-srfi-info
  (curry web-api-srfi-file web-text-bytes-response "-info.scm"))

(define web-api-srfi-args
  (curry web-api-srfi-file web-text-bytes-response "-args.scm"))

(define web-api-srfi-html
  (curry web-api-srfi-file web-html-bytes-response ".html"))

(define-values (web-dispatch web-url)
  (dispatch-rules
   [("")
    web-main-page]
   [("api" "v0" "srfi" (integer-arg) "info") web-api-srfi-info]
   [("api" "v0" "srfi" (integer-arg) "args") web-api-srfi-args]
   [("api" "v0" "srfi" (integer-arg) "html") web-api-srfi-html]
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

(database-initialize database-url)
(web-serve)
