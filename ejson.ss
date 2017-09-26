(library (ejson)
  (export json-ref
	  scm->json-file
          json-file->scm)

  (import (chezscheme) (json) (strings))

   (define json-ref
    (lambda (json k . ks)
      (cond ((hashtable? json)
	     (cond ((null? k) json)
		   ((null? ks) (hashtable-ref json (if (number? k) (string->symbol (number->string k)) k) #f))
		   (else (apply json-ref (hashtable-ref json (if (number? k) (string->symbol (number->string k)) k) #f) (car ks) (cdr ks)))))
	    ((list? json)
	     (cond ((null? k) json)
		   ((null? ks) (list-ref json k))
		   (else (apply json-ref (list-ref json k) (car ks) (cdr ks)))))
	    (else (cond ((null? k) json)
			(else (error 'json-ref (format "key: ~s not exists in last key's value ~s" k json))))))))
  
    (define json-file->scm
      (lambda (file)
        (json-string->scm (file->string file))))

    (define scm->json-file
      (lambda (json file pretty)
	(string->file (scm->json-string json #f pretty) file)))
  )
