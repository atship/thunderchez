(library (ejson)
  (export json-ref
          json-file->scm)

  (import (chezscheme) (json))

   (define json-ref
    (lambda (json k . ks)
      (cond ((hashtable? json)
	     (cond ((null? k) json)
		   ((null? ks) (hashtable-ref json k #f))
		   (else (apply json-ref (hashtable-ref json k #f) (car ks) (cdr ks)))))
	    ((list? json)
	     (cond ((null? k) json)
		   ((null? ks) (list-ref json k))
		   (else (apply json-ref (list-ref json k) (car ks) (cdr ks)))))
	    (else (cond ((null? k) json)
			(else (error 'json-ref (format "key: ~s not exists in last key's value ~s" k json))))))))
  
    (define json-file->scm
      (lambda (file)
        (json-string->scm (read-file file))))

    (define (read-file filename)
      (with-input-from-file filename
        (lambda ()
          (let loop ([x (read-char)] [acc '()])
            (if (eof-object? x) (apply string (reverse acc))
                (loop (read-char) (cons x acc)))))))
    
  )
