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

(define (cleanup-srfi-title title)
  (match (regexp-match #rx"^SRFI[ -][0-9]+: (.*)$" title)
    ((list _ real-title) real-title)
    (else title)))

(define (parse-srfi-info info-sexp-bytes)
  (let* ((raw (call-with-input-string (bytes->string/utf-8 info-sexp-bytes) read))
         (alist (if (pair? raw) (cdr raw) '()))
         (table (make-hash)))
    (hash-set! table 'title
               (cleanup-srfi-title (cadr (or (assoc 'title alist) '(title "")))))
    (hash-set! table 'status "final")
    table))

(define (srfi-html-url srfi-number)
  (format "/file/srfi-~a.html" srfi-number))

(define (srfi-info-url srfi-number)
  (format "/file/srfi-~a-info.scm" srfi-number))

(define (srfi-args-url srfi-number)
  (format "/file/srfi-~a-args.scm" srfi-number))

(define (web-main-page req)
  (let ((srfi-table (database-get-srfi-table)))
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
           (th "Info")
           (th "Args")
           (th "Types"))
          ,@(map (lambda (srfi-number)
                   (let* ((srfi-files-1 (hash-ref srfi-table srfi-number))
                          (info (parse-srfi-info
                                 (hash-ref srfi-files-1 "-info.scm" #""))))
                     `(tr
                       (td ,(number->string srfi-number))
                       (td ,(hash-ref info 'title))
                       (td ((class ,(string-join
                                     (list "srfi-status"
                                           (hash-ref info 'status))))))
                       (td ((class ,(string-join '("volunteer-status" "ok"))))
                           (a ((href ,(srfi-html-url srfi-number))) "html"))
                       (td ((class ,(string-join '("volunteer-status" "ok"))))
                           (a ((href ,(srfi-info-url srfi-number))) "scm"))
                       (td ((class ,(string-join '("volunteer-status" "ok"))))
                           (a ((href ,(srfi-args-url srfi-number))) "scm"))
                       (td ((class ,(string-join '("volunteer-status"
                                                   "pending"))))))))
                 (sort (hash-keys srfi-table) <)))))))))

(define (web-api-srfi-file send-response srfi-number srfi-suffix)
  (let ((contents (database-get-srfi-file srfi-number srfi-suffix)))
    (if (not contents)
        (web-error-response 404 "We don't have that SRFI or that metadata file")
        (send-response contents))))

(define (web-file req filename)
  (let ((we-dont "We don't have any filenames of that form"))
    (match (regexp-match #rx"^srfi-(0|[1-9][0-9]*)([.-].*)$" filename)
      ((list _ srfi-number srfi-suffix)
       (let ((srfi-number (string->number srfi-number)))
         (case srfi-suffix
           ((".html")
            (web-api-srfi-file web-html-bytes-response srfi-number ".html"))
           (("-args.scm")
            (web-api-srfi-file web-text-bytes-response srfi-number "-args.scm"))
           (("-info.scm")
            (web-api-srfi-file web-text-bytes-response srfi-number "-info.scm"))
           (else
            (web-error-response 404 we-dont)))))
      (else
       (web-error-response 404 we-dont)))))

(define-values (web-dispatch web-url)
  (dispatch-rules
   [("")
    web-main-page]
   [("file" (string-arg)) web-file]
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
