(library 
  (object)
  (export
    obj
    obj-has?; o k
    obj-ref; o k
    obj-set! ; o k v
    obj-rm! ; o k
    obj-for ; o (lambda (p) ...)
    obj-map ; o (lambda (p) ...)
    array
    array-map ; a (lambda (i v))
    array-for ; a (lambda (i v))
    array-size
    array-ref
    array-set!
    array-push
    array-pop
    array-in
    array-out
    empty?
    )
  (import (chezscheme) (script))

  (define (empty? ao)
    (or (not ao)
        (= 1 (length ao))))

  (define (obj)
    (make-list 1 '$obj$))

  (define array
    (case-lambda
      [() (make-list 1 '$array$)]
      [(l) (array l 0)]
      [(l v) 
       (let ([a (make-list (+ 1 l) v)])
         (set-car! a '$array$)
         a)]))

  (define (array-for a f)
    (for-each f (iota (length (cdr a))) (cdr a)))

  (define (array-map a f)
    (for f (iota (length (cdr a))) (cdr a)))

  (define (array-size a)
    (- (length a) 1))

  (define (array-ref a i)
    (list-ref a (+ 1 i)))

  (define (array-set! a i v)
    (list-set! a (+ 1 i) v))

  (define (array-push a v)
    (list-append! a v))

  (define (array-pop a)
    (if (empty? a)
      (eof)
      (let ([v (list-ref a (- (length a) 1))])
        (list-remove! a -1)
        v)))

  (define-syntax array-in (identifier-syntax array-push))
  (define (array-out a)
    (if (empty? a)
      (eof)
      (let ([v (list-ref a 1)])
        (list-remove! a 1)
        v)))

  (define (obj-has? o k)
    (if (assq k (cdr o))
      #t
      #f))

  (define (obj-ref o k)
    (let ([f (assoc k (cdr o))])
      (if f (cadr f)
        #f)))

  (define (obj-for o l)
    (for-each l (cdr o)))

  (define (obj-map o l)
    (map l (cdr o)))

  (define (obj-set! o k v)
    (let ([f (assoc k (cdr o))])
      (if f 
        (set-cdr! f (cons v '()))
        (let ([f (list k v)])
          (set-cdr! (list-tail o (- (length o) 1)) (list f))))))

  (define (obj-rm! o k)
    (let ([idx (find (lambda (i)
                       (equal? k (car (list-ref o (+ 1 i)))))
                     (iota (length (cdr o))))])
      (if idx
        (list-remove! o (+ 1 idx)))))
  )
