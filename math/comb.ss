(library (math comb)
  (export
   A
   C
   AA
   CC
   P
   factorial)
  (import (chezscheme))

  (define (factorial n)
    (define (S r n)
      (if (<= n 1)
	  r
	  (S (* r n) (- n 1))))
    (S 1 n))

  (define (P l r . z)
    (define (combine l r kons)
      (apply append (map (lambda (x)
	      (map (lambda (y)
		     (kons x `(,y)))
		   r))
	    l)))
    (let ([r (combine l r cons)])
      (for-each (lambda (x)
		  (set! r (combine r x append)))
		z)
      r))
  
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

  (define (AA lst n)
    (let ([len (length lst)])
      (if (< len n)
	  (error 'AA "length of list is less" n)
	  (apply append (map (lambda (x)
		  (if (<= n 1)
		      `((,x))
		      (map (lambda (y)
			     (cons x y)) (AA (remove x lst) (- n 1)))))
			     lst)))))
  (define (CC lst n)
    (define (inset? s lst)
      (exists (lambda (x)
		(if (= (length x) (length s))
		    (not (exists (lambda (y)
			       (not (memq y x)))
			     s))
		    #f))
	      lst))
    (let ([s (AA lst n)])
      (reverse (fold-left (lambda (r x)
			    (if (inset? x r)
				r
				(cons x r)))
			  '() s))))
  )
