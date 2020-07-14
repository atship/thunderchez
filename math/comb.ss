(library (math comb)
  (export
   A
   C
   factorial)
  (import (chezscheme))

  (define (factorial n)
    (define (S r n)
      (if (<= n 1)
	  r
	  (S (* r n) (- n 1))))
    (S 1 n))

  (define (A n m)
    (define (f r n e)
      (if (<= n e)
	  (* r n)
	  (f (* r n) (- n 1) e)))
    (if (or (<= n 0)
	    (<= m 0)
	    (< n m))
	1
	(f 1 n (- n (- m 1)))))

  (define (C n m)
    (if (< (/ n 2) m)
      (set! m (- n m)))
    (/ (A n m)
       (A m m)))
  )
