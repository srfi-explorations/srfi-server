#lang racket

(require
 crypto
 crypto/libcrypto
 db/base
 db/postgresql
 file/unzip
 json
 net/url
 racket/match
 racket/string
 web-server/servlet
 web-server/servlet-env)

(crypto-factories libcrypto-factory)

;;;

(define (must-env envar)
  (let ((val (getenv envar)))
    (if (and val (not (= 0 (string-length val))))
        val
        (error (string-append envar " environment variable not set")))))

(define web-port
  (string->number (must-env "PORT")))

(define database-url
  (string->url (must-env "DATABASE_URL")))

(define github-webhook-secret
  (string->bytes/utf-8 (must-env "GITHUB_WEBHOOK_SECRET")))

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

(define (web-unauthorized req)
  (web-error-response 403 "Forbidden"))

(define (github-sha1 byts)
  (bytes-append #"sha1="
                (string->bytes/utf-8
                 (bytes->hex-string
                  (hmac 'sha1 github-webhook-secret byts)))))

(define (github-sha1-match? req req-bytes)
  (let* ((sig-head* (headers-assq* #"X-Hub-Signature" (request-headers/raw req)))
         (sig-head (if sig-head* (header-value sig-head*) #""))
         (sig-body (github-sha1 req-bytes)))
    (bytes=? sig-head sig-body)))

(define (github-archive-url repository commit-hash)
  (let ((u (hash-ref repository 'archive_url)))
    (set! u (string-replace u "{archive_format}" "zipball"))
    (set! u (string-replace u "{/ref}" (string-append "/" commit-hash)))
    u))

(define (web-admin-github req)
  (let* ((req-bytes (request-post-data/raw req)))
    (cond ((not (github-sha1-match? req req-bytes))
           (web-unauthorized req))
          (else
           (let ((j (bytes->jsexpr req-bytes)))
             (display j (current-error-port))
             (display #\newline (current-error-port))
             (let* ((repository (hash-ref j 'repository))
                    (repo-name (hash-ref repository 'name)))
               (fprintf (current-error-port)
                        "The repo name is <~a>~n" repo-name)
               (match (regexp-match #rx"^srfi-([1-9]\\d*)$" repo-name)
                 ((list _ srfi-number)
                  (let ((srfi-number (string->number srfi-number)))
                    (fprintf (current-error-port)
                             "The repo SRFI number is <~a>~n" srfi-number)))
                 (else
                  (fprintf (current-error-port) "This is not a SRFI repo")))
               (fprintf (current-error-port)
                        "The archive download link is <~a>~n"
                        (let ((commit-hash (hash-ref j 'after)))
                          (github-archive-url repository commit-hash)))
               (response/xexpr '(html (body (h1 "OK"))))))))))

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
