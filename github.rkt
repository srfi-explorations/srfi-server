#lang racket

(require
 crypto
 crypto/libcrypto
 web-server/servlet
 "util.rkt")

(crypto-factories libcrypto-factory)

(define github-webhook-secret
  (string->bytes/utf-8 (must-env "GITHUB_WEBHOOK_SECRET")))

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

(define (github-zip-url repository commit-hash)
  (let ((u (hash-ref repository 'archive_url)))
    (set! u (string-replace u "{archive_format}" "zipball"))
    (set! u (string-replace u "{/ref}" (string-append "/" commit-hash)))
    u))

(provide github-sha1-match? github-zip-url)
