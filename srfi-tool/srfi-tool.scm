(import
 (scheme base)
 (scheme r5rs)
 ;;(srfi 1)
 ;;(srfi 6)
 ;;(srfi 23)
 ;;(srfi 130)
 )

;; (chibi html-parser)

;;

(define *debug* #t)

(define-syntax debug
  (syntax-rules ()
    ((_ strings ...)
     (begin (if *debug*
                (display (string-append "debug: " strings ... (string #\newline))
                         (current-error-port)))
            #f))))

;;

(define eof 'eof)

(define (constantly x)
  (lambda _ x))

(define (for-each-with-index list mapfun)
  (let loop ((i 0) (list list))
    (if (pair? list)
        (begin (mapfun i (car list))
               (loop (+ i 1) (cdr list))))))

(define (string-has-char? string ch)
  (let ((found? #f))
    (string-for-each (lambda (c) (set! found? (or found? (equal? ch c))))
                     string)
    found?))

(define (print-to-string x)
  (let ((out (open-output-string)))
    (display x out)
    (get-output-string out)))

(define (display-list list)
  (for-each-with-index
   list
   (lambda (i x)
     (display (if (= i 0) "(" " "))
     (display x)
     (if (= i (- (length list) 1))
         (display ")"))
     (newline))))

;;

(define (sxml-for-each proc elem)
  (cond ((not (pair? elem)) '())
        ((equal? '@ (car elem)) '())
        (else (proc elem)
              (for-each (lambda (x) (sxml-for-each proc x))
                        (cdr elem)))))

(define (sxml-text elem)
  (cond ((string? elem) elem)
        ((not (pair? elem)) "")
        ((equal? '@ (car elem)) "")
        (else (string-append (sxml-text (car elem))
                             (sxml-text (cdr elem))))))

(define (sxml-attributes elem)
  (if (and (pair? elem)
           (pair? (cdr elem))
           (pair? (cadr elem))
           (equal? '@ (caadr elem)))
      (cdadr elem)
      '()))

;;

(define (make-string-reader s)
  (let ((i 0))
    (lambda (k)
      (let* ((char (if (< i (string-length s))
                       (string-ref s i)
                       eof))
             (match? (cond ((procedure? k) (k char))
                           (else (equal? k char)))))
        (cond ((not match?) #f)
              (else (set! i (min (+ i 1) (string-length s)))
                    char))))))

(define (read-while* rd k)
  (let ((out (open-output-string)))
    (let loop ()
      (let ((char (rd k)))
        (if (not char)
            (let ((outs (get-output-string out)))
              (if (= 0 (string-length outs)) #f outs))
            (begin (write-char char out)
                   (loop)))))))

(define (skip-whitespace* rd)
  (let loop ()
    (if (rd (lambda (c) (and (char? c) (char-whitespace? c))))
        (loop))))

;;

(define symbol-safe
  (string-append "0123456789"
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 "abcdefghijklmnopqrstuvwxyz"
                 "<>=/*+-?!.#"))

(define (symbol-char? ch)
  (and (char? ch) (string-has-char? symbol-safe ch)))

(define (read-symbol rd)
  (read-while* rd symbol-char?))

(define (read-things rd closing)
  (let loop ((things '()))
    (skip-whitespace* rd)
    (if (rd closing)
        (reverse things)
        (loop (cons (read-sexp rd) things)))))

(define (read-list rd opening closing constructor)
  (if (rd opening) (constructor (read-things rd closing)) #f))

(define (read-sexp rd)
  (skip-whitespace* rd)
  (cond ((read-list rd #\( #\) list))
        ((read-list rd #\[ #\] (lambda xs (cons 'optional xs))))
        ((read-symbol rd))
        (else (error (string-append "Syntax error. Next char is "
                                    (print-to-string (rd (constantly #t)))
                                    ".")))))

(define (read-all-sexps rd)
  (read-things rd (lambda (ch) (equal? eof ch))))

;;

(define (string-prefix? small big)
  (and (>= (string-length big) (string-length small))
       (string=? small (substring big 0 (string-length small)))))

(define (string-suffix? small big)
  (and (>= (string-length big) (string-length small))
       (string=? small (substring big (- (string-length big)
                                         (string-length small))))))

(define (string-split-at-spaces s)
  (let ((n (string-length s)))
    (let loop ((a 0) (parts '()))
      (cond ((>= a n)
             (reverse parts))
            ((char-whitespace? (string-ref s a))
             (loop (+ a 1) parts))
            (else
             (let ((b (let inner-loop ((b a))
                        (cond ((>= b n) n)
                              ((char-whitespace? (string-ref s b)) b)
                              (else (inner-loop (+ b 1)))))))
               (loop b (cons (substring s a b) parts))))))))

(define (string-trim-both s predicate)
  (let ((n (string-length s)))
    (let ((a (let loop ((a 0))
               (cond ((>= a n) n)
                     ((predicate (string-ref s a)) (loop (+ a 1)))
                     (else a))))
          (b (let loop ((b n))
               (cond ((<= b 0) 0)
                     ((predicate (string-ref s (- b 1))) (loop (- b 1)))
                     (else b)))))
      (substring s a b))))

(define (filter predicate list)
  (let loop ((list list) (acc '()))
    (if (null? list)
        (reverse acc)
        (loop (cdr list)
              (if (predicate (car list)) (cons (car list) acc) acc)))))

(define (string-remove predicate s)
  (list->string (filter (lambda (x) (not (predicate x))) (string->list s))))

(define (remove predicate list)
  (filter (lambda (x) (not (predicate x))) list))

(define (html-classes elem)
  (let ((classes (assoc 'class (sxml-attributes elem))))
    (string-split-at-spaces (if classes (cadr classes) ""))))

(define (cleanup s)
  (string-trim-both
   (string-remove (lambda (ch)
                    (or (equal? ch #\newline)
                        (equal? ch #\return)))
                  s)
   char-whitespace?))

(define (parse-arg-list args flags)
  (let ((full-list '())
        (last-arrow #f))
    (for-each-with-index
     args
     (lambda (i arg)
       (cond ((equal? "->" arg)
              (set! last-arrow i))
             ((list? arg)
              (let ((retflags (if (equal? last-arrow (- i 1)) '(return) '())))
                (cond ((equal? 'optional (car arg))
                       (set! full-list
                             (append full-list
                                     (parse-arg-list
                                      (cdr arg)
                                      (append flags retflags '(optional))))))
                      ((not (member 'syntax flags))
                       (set! full-list
                             (append full-list
                                     `((sublist
                                        ,@(parse-arg-list
                                           arg (append flags retflags))))))))))
             (else
              (if (not (or (string? arg) (symbol? arg)))
                  (error (string-append "Expected symbol in arglist but got "
                                        (print-to-string arg))))
              (let ((argflags (remove (lambda (x) (equal? 'syntax x)) flags))
                    (which 'arg))
                (if (or (member 'return argflags)
                        (equal? last-arrow (- i 1)))
                    (set! which 'return))
                (cond ((string-suffix? "..." arg)
                       (set! argflags
                             (append (remove (lambda (x) (equal? 'optional x))
                                             argflags)
                                     '(rest))))
                      ((and (string-prefix? "<" arg)
                            (string-suffix? ">" arg)
                            (> (string-length arg) 2))
                       (set! arg (substring arg 1 (- (string-length arg) 1)))
                       (if (member 'syntax flags)
                           (set! which 'arg)))
                      ((member 'syntax flags)
                       (set! which 'quoted-symbol)))
                (if (equal? 'return which)
                    (set! argflags
                          (remove (lambda (x) (or (equal? 'optional x)
                                                  (equal? 'return x)))
                                  argflags)))
                (set! full-list
                      (append full-list
                              `((,which ,arg ,@argflags)))))))))
    full-list))

(define (parse-proc-def s syntax?)
  (debug "parse-proc-def: " (print-to-string s))
  (let ((things (read-all-sexps (make-string-reader s))))
    (if (and (pair? things) (pair? (car things)) (pair? (caar things)))
        (set! things (caar things)))
    (if (not (pair? things))
        '()
        `(,(if syntax? 'syntax 'procedure)
          ,(car things)
          ,@(parse-arg-list (cdr things) (if syntax? '(syntax) '()))))))

(define (for-each-def proc sxml)
  (sxml-for-each (lambda (elem)
                   (let ((classes (html-classes elem)))
                     (if (member "def"  classes)
                         (let ((text (cleanup (sxml-text elem)))
                               (type (cond ((member "variable" classes) 'variable)
                                           ((member "syntax" classes) 'syntax)
                                           ((member "proc" classes) 'proc))))
                           (proc text type)))))
                 sxml))

;;

(define (process-html-port html-port)
  (for-each-def (lambda (text type)
                  (case type
                    ('syntax
                     (display-list (parse-proc-def text #t)))
                    ('proc
                     (display-list (parse-proc-def text #f)))
                    ('variable
                     (display-list
                      `(variable ,@(read-all-sexps (make-string-reader text)))))
                    (else
                     (error "Unknown def type")))
                  (newline))
                (html->sxml html-port)))

(define (process-html-file html-filename)
  (call-with-input-file html-filename process-html-port))

(define (main arguments)
  (for-each process-html-file (cdr arguments)))
