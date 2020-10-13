#!chezscheme
(library (strings)
         (export string-replace
                 string-replace/all
                 string-split
                 string-join
                 string->file
                 string-starts?
                 string-starts-ci?
                 string-ends?
                 string-ends-ci?
                 file->string)

         (import (chezscheme) (irregex) (srfi private let-opt))

         (define (string-starts? pattern str)
           (let loop ((i 0))
             (cond
               ((>= i (string-length pattern)) #t)
               ((>= i (string-length str)) #f)
               ((char=? (string-ref pattern i) (string-ref str i))
                (loop (+ 1 i)))
               (else #f))))

         (define (string-starts-ci? pattern str)
           (let loop ((i 0))
             (cond
               ((>= i (string-length pattern)) #t)
               ((>= i (string-length str)) #f)
               ((char-ci=? (string-ref pattern i) (string-ref str i))
                (loop (+ 1 i)))
               (else #f))))

         ; -- procedure+: string-suffix? PATTERN STRING
         ; -- procedure+: string-suffix-ci? PATTERN STRING
         ; checks to make sure that PATTERN is a suffix of STRING
         ;
         ;          (string-suffix? "ate" "pirate")             =>  #t
         ;          (string-suffix? "rag" "outrage")            =>  #f
         ;          (string-suffix? "" any-string)              =>  #t
         ;          (string-suffix? any-string any-string)      =>  #t

         (define (string-ends? pattern str)
           (let loop ((i (1- (string-length pattern))) (j (1- (string-length str))))
             (cond
               ((negative? i) #t)
               ((negative? j) #f)
               ((char=? (string-ref pattern i) (string-ref str j))
                (loop (1- i) (1- j)))
               (else #f))))

         (define (string-ends-ci? pattern str)
           (let loop ((i (1- (string-length pattern))) (j (1- (string-length str))))
             (cond
               ((negative? i) #t)
               ((negative? j) #f)
               ((char-ci=? (string-ref pattern i) (string-ref str j))
                (loop (1- i) (1- j)))
               (else #f))))

         (define (file->string filename)
           (call-with-input-file filename
                                 (lambda (p)
                                   (get-string-all p))))

         (define string->file
           (case-lambda
             [(s filename) (string->file s filename 'truncate)]
             [(s filename opt)
              (call-with-output-file filename
                                     (lambda (p)
                                       (put-string p s))
                                     `(,opt))]))

         (define string-replace
           (lambda (s s1 s2)
             (irregex-replace s1 s s2)))

         (define string-replace/all
           (lambda (s s1 s2)
             (irregex-replace/all s1 s s2)))

         (define string-split
           (lambda (s s1 . o)
             (apply irregex-split s1 s o)))

         ; Alas, Scheme 48's APPLY blows up if you have many, many arguments.
         ;(define (string-concatenate strings) (apply string-append strings))

         ;;; Here it is written out. I avoid using REDUCE to add up string lengths
         ;;; to avoid non-R5RS dependencies.
         (define (string-concatenate strings)
           (let* ((total (do ((strings strings (cdr strings))
                              (i 0 (+ i (string-length (format "~a" (car strings))))))
                           ((not (pair? strings)) i)))
                  (ans (make-string total)))
             (let lp ((i 0) (strings strings))
               (if (pair? strings)
                 (let* ((s (format "~a" (car strings)))
                        (slen (string-length s)))
                   (%string-copy! ans i s 0 slen)
                   (lp (+ i slen) (cdr strings)))))
             ans))

         ;;; Library-internal routine
         (define (%string-copy! to tstart from fstart fend)
           (if (> fstart tstart)
             (do ((i fstart (+ i 1))
                  (j tstart (+ j 1)))
               ((>= i fend))
               (string-set! to j (string-ref from i)))

             (do ((i (- fend 1) (- i 1))
                  (j (+ -1 tstart (- fend fstart)) (- j 1)))
               ((< i fstart))
               (string-set! to j (string-ref from i)))))


         ;;; (string-join string-list [delimiter grammar]) => string
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;; Paste strings together using the delimiter string.
         ;;;

         ;;; (join-strings '("foo" "bar" "baz") ":") => "foo:bar:baz"
         ;;;
         ;;; DELIMITER defaults to a single space " "
         ;;; GRAMMAR is one of the symbols {prefix, infix, strict-infix, suffix}
         ;;; and defaults to 'infix.
         ;;;
         ;;; I could rewrite this more efficiently -- precompute the length of the
         ;;; answer string, then allocate & fill it in iteratively. Using
         ;;; STRING-CONCATENATE is less efficient.

         (define (string-join strings . delim+grammar)
           (let-optionals* delim+grammar ((delim " " (string? delim))
                                          (grammar 'infix))
                           (let ((buildit (lambda (lis final)
                                            (let recur ((lis lis))
                                              (if (pair? lis)
                                                (cons delim (cons (format "~a" (car lis)) (recur (cdr lis))))
                                                final)))))

                             (cond ((pair? strings)
                                    (string-concatenate
                                      (case grammar

                                        ((infix strict-infix)
                                         (cons (car strings) (buildit (cdr strings) '())))

                                        ((prefix) (buildit strings '()))

                                        ((suffix)
                                         (cons (car strings) (buildit (cdr strings) (list delim))))

                                        (else (error "Illegal join grammar"
                                                     grammar string-join)))))

                                   ((not (null? strings))
                                    (error "STRINGS parameter not list." strings string-join))

                                   ;; STRINGS is ()

                                   ((eq? grammar 'strict-infix)
                                    (error "Empty list cannot be joined with STRICT-INFIX grammar."
                                           string-join))

                                   (else ""))))) ; Special-cased for infix grammar.
         )
