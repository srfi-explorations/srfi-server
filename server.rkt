#lang racket

(require
 file/unzip
 json
 net/url
 web-server/servlet
 web-server/servlet-env)

(define (start req)
  (response/xexpr
   '(html (head (title "Racket Heroku App"))
          (body (h1 "It works!")))))

(define port (if (getenv "PORT")
                 (string->number (getenv "PORT"))
                 (error "PORT environment variable not set")))

(serve/servlet start
               #:servlet-path "/"
               #:listen-ip #f
               #:port port)
