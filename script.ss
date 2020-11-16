#!chezscheme

(library
  (script)
  (export println
          echo
          ls
          shell

          string-replace
          string-replace/all
          string-split
          string-join
          string-starts?
          string-starts-ci?
          string-ends?
          string-ends-ci?
          string->file
          include-file
          file->string
	  file->list
    file->bytes
    walkdir
          file->utf8-iport
          file->utf8-oport
          file->iport
          file->oport
          file-iclose
          file-oclose
          fclose
          fopen
          string->iport
          string->oport
          string->code
          eval-string
          eval-list

          json-ref
          json->file
          file->json
          string->json
          json->string
          json->string2

          A
          C
	  AA
	  CC
          P

          loge
          logw
          logi
          logd
          tab
          int

          match
          match-lambda
          match-lambda*
          match-let
          match-let*
          match-letrec
          match-named-let

          read-bytes
          write-bytes
          read-int
          read-uint
          read-long
          read-ulong
          read-short
          read-ushort
          read-byte
          read-bool
          read-ubyte
          read-utf8
          read-sym
          read-float
          read-double
          write-utf8
          write-sym
          write-uint
          write-int
          write-ulong
          write-long
          write-ushort
          write-short
          write-ubyte
          write-bool
          write-byte
          write-float
          write-double
          eof?

          string->xml
          file->xml
          xml->file
          xml->string
          xml-filter
          xml-ref
          map1
	  list-append
	  list-append!
          list-set!
	  list-insert!
          list-remove!
          first
          second
          third
          forth
          nth

          make-hashmap

          mkdirs
          )

  (import (chezscheme) (strings) (ejson) (xtool) (matchable) (math comb) (sxml))
  (define (walkdir dir fun)
    (let ([cdir (cd)])
      (cd dir)
      (let ([fs (shell "ls | sort -V")])
        (for-each (lambda(f i)
                    (if (file-directory? f)
                      (walkdir f fun)
                      (fun f i)))
             fs (iota (length fs))))
      (cd cdir)))
  (define (file->bytes f)
    (letrec* ([ff (file->iport f)] [bytes (get-bytevector-all ff)])
      (close-port ff)
      bytes))
  (define file->list
   (lambda (fname)
    (let ([file (open-input-file fname)])
      (let ([data
             (let recurse ([decl (read file)])
               (if (eof-object? decl)
                 '()
                 (cons decl (recurse (read file)))))])
        (close-input-port file)
	data))))
  (define (include-file f)
     (for-each (lambda (x) (eval x)) (file->list f)))

  (define fopen
    (case-lambda
      [(f) (fopen f 'r)]
      [(f r?)
       (case r?
         [r (file->utf8-iport f)]
         [rb (file->iport f)]
         [w (file->utf8-oport f)]
         [wb (file->oport f)])]))
  (define-syntax fclose (identifier-syntax close-port))
  (define-syntax first (identifier-syntax car))
  (define-syntax second (identifier-syntax cadr))
  (define-syntax third (identifier-syntax caddr))
  (define-syntax forth (identifier-syntax cadddr))
  (define-syntax nth (identifier-syntax list-ref))

  (define (list-set! ls n v)
    (set-car! (list-tail ls n) v)
    ls)
  (define (list-append a . b)
    (append a b))
  (define (list-append! a . b)
    (for-each
     (lambda (x)
       (list-insert! a -1 x))
     b))

  (define (list-insert! ls n v)
    (if (< (length ls) n)
	(set! n (length ls)))
    (cond
     [(= n 0)
      (set-cdr! ls (cons (car ls) (cdr ls)))
      (set-car! ls v)
      ls]
     [(< n 0)
      (list-insert! ls (+ 1 n (length ls)) v)]
     [else
      (set-cdr! (list-tail ls (- n 1)) (cons v (list-tail ls n)))
      ls]))

  (define (list-remove! ls n)
    (if (<= (length ls) n)
	(set! n (- (length ls) 1)))
    (cond
     [(= 0 n)
      (set-car! ls (cadr ls))
      (set-cdr! ls (cddr ls))
      ls]
     [(< n 0)
      (list-remove! ls (+ n (length ls)))]
     [else
      (set-cdr! (list-tail ls (- n 1)) (list-tail ls (+ n 1)))
      ls]))

  (define map1
    (lambda (f ls . more)
      (if (null? more)
          (let map2 ([ls ls])
            (if (null? ls)
                '()
                (cons (f (car ls))
                      (map2 (cdr ls)))))
          (let map-more ([ls ls] [more more])
            (if (null? ls)
                '()
                (cons
                  (apply f (car ls) (map1 car more))
                  (map-more (cdr ls) (map1 cdr more))))))))

  (define (int i)
    (inexact->exact (round i)))

  (define (string->code s)
    (read (string->iport s)))

  (define (eval-string s)
    (eval (string->code s)))

  (define (eval-list ls)
    (eval-string (format "~a" ls)))

  (define (shell f . args)
    (let* ([r (process (apply format f args))]
	     [p (car r)]
	     [pid (caddr r)])
      (let loop ([rs ""]
		 [rss (get-string-all p)])
	(if (eof? rss)
	    (begin
	      (system (format "kill -9 ~a" pid))
	      (string-split rs "\n"))
	    (loop (string-append rs rss)
		  (get-string-all p))))))

  (define (echo o . args)
    (printf "~a\n" o)
    (if (not (null? args))
        (apply echo (car args) (cdr args))))

  (define (println f . args)
    (apply printf (format "~a\n" f) args))

  (define (file->utf8-iport f)
    (open-file-input-port f (file-options) 'block (make-transcoder (utf-8-codec))))

  (define (file->utf8-oport f)
    (open-file-output-port f (file-options replace) 'block (make-transcoder (utf-8-codec))))

  (define-syntax file->iport (identifier-syntax open-file-input-port))

  (define (file->oport f)
    (open-file-output-port f (file-options replace) 'block))

  (define (eof? f)
    (if (port? f)
      (port-eof? f)
      (eof-object? f)))

  (define-syntax file-iclose (identifier-syntax close-input-port))

  (define-syntax file-oclose (identifier-syntax close-output-port))

  (define-syntax string->iport (identifier-syntax open-string-input-port))

  (define-syntax string->oport (identifier-syntax open-string-output-port))

  (define-syntax ls (identifier-syntax directory-list))

  (define-syntax read-bytes (identifier-syntax get-bytevector-n))

  (define-syntax write-bytes (identifier-syntax put-bytevector))

  (define read-int
    (case-lambda
      [(port) (read-int port #f)]
      [(port big) (bytevector-s32-ref (read-bytes port 4) 0 (if big 'big 'little))]))

  (define read-uint
    (case-lambda
      [(port) (read-uint port #f)]
      [(port big) (bytevector-u32-ref (read-bytes port 4) 0 (if big 'big 'little))]))

  (define read-long
    (case-lambda
      [(port) (read-long port #f)]
      [(port big) (bytevector-s64-ref (read-bytes port 8) 0 (if big 'big 'little))]))

  (define read-ulong
    (case-lambda
      [(port) (read-ulong port #f)]
      [(port big) (bytevector-u64-ref (read-bytes port 8) 0 (if big 'big 'little))]))

  (define read-short
    (case-lambda
      [(port) (read-short port #f)]
      [(port big) (bytevector-s16-ref (read-bytes port 2) 0 (if big 'big 'little))]))

  (define read-ushort
    (case-lambda
      [(port) (read-ushort port #f)]
      [(port big) (bytevector-u16-ref (read-bytes port 2) 0 (if big 'big 'little))]))

  (define (read-byte port)
    (bytevector-s8-ref (read-bytes port 1) 0))

  (define (read-bool port)
    (= 0 (bytevector-s8-ref (read-bytes port 1) 0)))

  (define (read-ubyte port)
    (bytevector-u8-ref (read-bytes port 1) 0))

  (define read-float
    (case-lambda
      [(port) (read-float port #f)]
      [(port big) (bytevector-ieee-single-ref (read-bytes port 4) 0 (if big 'big 'little))]))

  (define read-double
    (case-lambda
      [(port) (read-double port #f)]
      [(port big) (bytevector-ieee-double-ref (read-bytes port 8) 0 (if big 'big 'little))]))

  (define write-uint
    (case-lambda
      [(port i) (write-uint port i #f)]
      [(port i big) (let ([bytes (make-bytevector 4)])
                      (bytevector-u32-set! bytes 0 i (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-int
    (case-lambda
      [(port i) (write-int port i #f)]
      [(port i big) (let ([bytes (make-bytevector 4)])
                      (bytevector-s32-set! bytes 0 i (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-ulong
    (case-lambda
      [(port i) (write-ulong port i #f)]
      [(port i big) (let ([bytes (make-bytevector 8)])
                      (bytevector-u64-set! bytes 0 i (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-long
    (case-lambda
      [(port i) (write-long port i #f)]
      [(port i big) (let ([bytes (make-bytevector 8)])
                      (bytevector-s64-set! bytes 0 i (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-short
    (case-lambda
      [(port i) (write-short port i #f)]
      [(port i big) (let ([bytes (make-bytevector 2)])
                      (bytevector-s16-set! bytes 0 i (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-ushort
    (case-lambda
      [(port s) (write-ushort port s #f)]
      [(port s big) (let ([bytes (make-bytevector 2)])
                      (bytevector-u16-set! bytes 0 s (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define (write-byte port b)
    (let ([bytes (make-bytevector 1)])
      (bytevector-s8-set! bytes 0 b)
      (write-bytes port bytes)))

(define (write-bool port b)
    (let ([bytes (make-bytevector 1)])
      (bytevector-s8-set! bytes 0 (if b 1 0))
      (write-bytes port bytes)))

  (define (write-ubyte port b)
    (let ([bytes (make-bytevector 1)])
      (bytevector-u8-set! bytes 0 b)
      (write-bytes port bytes)))

  (define write-float
    (case-lambda
      [(port f) (write-float port f #f)]
      [(port f big) (let ([bytes (make-bytevector 4)])
                      (bytevector-ieee-single-set! bytes 0 f (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define write-double
    (case-lambda
      [(port f) (write-double port f #f)]
      [(port f big) (let ([bytes (make-bytevector 8)])
                      (bytevector-ieee-double-set! bytes 0 f (if big 'big 'little))
                      (write-bytes port bytes))]))

  (define (read-sym port)
    (string->symbol (read-utf8 port)))

  (define read-utf8
    (case-lambda
      [(port) (let ([len (read-uint port)]) (read-utf8 port len))]
      [(port len) (utf8->string (read-bytes port len))]))

  (define (write-sym port s)
    (write-utf8 port (symbol->string s)))

  (define (write-utf8 port s)
    (let ([bytes (string->utf8 s)])
      (write-uint port (bytevector-length bytes))
      (write-bytes port bytes)))

  (define (mkdirs dir)
    (case (machine-type)
      [(ta6nt a6nt i3nt ti3nt)
       (shell "mkdir ~a" (string-replace/all dir "/" "\\"))]
      [else (shell "mkdir -p ~a" dir)]))

  (define string->xml
    (case-lambda
      [(f) (string->xml f '())]
      [(f ns) (ssax:xml->sxml (string->iport f) ns)]))

  (define file->xml
    (case-lambda
      [(f) (file->xml f '())]
      [(f ns) (ssax:xml->sxml (file->utf8-iport f) ns)]))

  (define (xml->file xml file)
    (string->file (xml->string xml) file))

  (define (xml->string xml)
    (define (has-attr cs)
      (and (pair? cs) (pair? (car cs)) (eqv? '@ (caar cs))))
    (define (attr ls port)
      (cond
        [(null? ls) #f]
        [(atom? (car ls))
         (if (null? (cdr ls))
             (put-string port (format " ~a=\"\"" (car ls)))
             (put-string port (format " ~a=\"~a\"" (car ls) (cadr ls))))]
        [else (attr (car ls) port)
              (attr (cdr ls) port)]))
    (define (child ls port)
      (cond
        [(null? ls) #f]
        [else
          (cond
            [(atom? (car ls))
             (put-string port (format "~a" (car ls)))] ; text
            [(eqv? '@ (caar ls))
             (attr (cdar ls) port)
             (if (null? (cdr ls))
                 (put-string port "/>")
                 (put-string port ">"))]
            [else (node (car ls) port)])
          (child (cdr ls) port)]))
    (define (node ls port)
      (let ([tag (car ls)] [children (cdr ls)])
        (cond
          [(eqv? tag '*TOP*) (child children port)]
          [(eqv? tag '*PI*)
           (put-string port (format "<?~a?>" (string-join children " ")))]
          [(null? children)
           (put-string port (format "<~a/>" tag))]
          [else
            (if (has-attr children)
                (begin
                  (put-string port (format "<~a" tag)))
                (put-string port (format "<~a>" tag)))
            (child children port)
            (if (or (and (= (length children) 1) (not (has-attr children)))
                    (> (length children) 1))
                (put-string port (format "</~a>" tag)))])))
    (let-values ([(port get) (string->oport)])
      (node xml port)
      (get)))

  (define (xml-filter xml path)
    ((sxpath path) xml))

  (define (xml-ref xml keys)
    (define (%xml-ref xml keys)
      (cond
        [(null? xml) '()]
        [(eq? (car xml) (car keys))
         (if (null? (cdr keys))
             xml
             (%xml-ref (cdr xml) (cdr keys)))]
        [(pair? (car xml))
         (let ([v (%xml-ref (car xml) keys)])
           (if (null? v)
               (%xml-ref (cdr xml) keys)
               v))]
        [else '()]))
    (%xml-ref (cdr xml) keys))

  (define make-hashmap
    (case-lambda
      [() (make-hashtable equal-hash equal?)]
      [(size) (make-hashtable equal-hash equal? size)]))

  (define json->string2
    (case-lambda
      [(json) (json->string2 json #t)]
      [(json pretty) (json->string2 json pretty (if pretty "\"~a\": " "\"~a\":"))]
      [(json pretty key-format)
       (define (print-pretty pretty port lv)
         (if pretty
             (put-string port (format (format "\n~~,,~a@a" (* 2 lv)) ""))))
       (define (value v port)
         (cond
           [(number? v)
            (put-string port (format "~a" v))]
           [(boolean? v)
            (put-string port (if v "true" "false"))]
           [else
             (put-string port (format "\"~a\"" v))]))
       (define (array lss port lv)
         (cond
           [(null? lss)
            (put-string port "[]")]
           [else
             (put-string port "[")
             (set! lv (+ 1 lv))
             (print-pretty pretty port lv)
             (let loop ([ls lss])
               (let ([v (car ls)])
                 (if (list? v)
                     (parse v port lv)
                     (value v port))
                 (if (null? (cdr ls))
                     (begin
                       (print-pretty pretty port (- lv 1))
                       (put-string port "]"))
                     (begin
                       (put-string port ",")
                       (print-pretty pretty port lv)
                       (loop (cdr ls))))))]))
       (define (obj lss port lv)
         (cond
           [(null? lss)
            (put-string port "{}")]
           [else
             (put-string port "{")
             (set! lv (+ 1 lv))
             (print-pretty pretty port lv)
             (let loop ([ls lss])
               (let ([k (car ls)]
                     [v (cadr ls)])
                 (put-string port (format key-format k))
                 (if (list? v)
                     (parse v port lv)
                     (value v port))
                 (if (null? (cddr ls))
                     (begin
                       (print-pretty pretty port (- lv 1))
                       (put-string port "}"))
                     (begin
                       (put-string port ",")
                       (print-pretty pretty port lv)
                       (loop (cddr ls))))))]))
       (define (parse ls port lv)
         (cond
           [(null? ls) (obj ls port lv)]
           [(atom? (car ls))
            (if (eq? '@ (car ls))
                (array (cdr ls) port lv)
                (obj ls port lv))]
           [else
             (println "error json format ~a" json)]))
       (let-values ([(port get) (string->oport)])
         (parse json port 0)
         (get))]))
  )
