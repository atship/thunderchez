(library (ejson)
  (export json-ref
          json->file
          file->json
          string->json
          json->string)

  (import (chezscheme) (json))

  (define string->json
    (lambda (s)
      (json-string->scm s)))

  (define json->string
    (case-lambda
      [(ss) (scm->json-string ss)]
      [(ss pretty) (scm->json-string ss #f pretty)]))

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

  (define (file->string filename)
    (call-with-input-file filename
      (lambda (p)
        (get-string-all p))))

  (define (string->file s filename)
    (delete-file filename)
    (call-with-output-file filename
      (lambda (p)
        (put-string p s))))

  (define file->json
    (lambda (file)
      (json-string->scm (file->string file))))

  (define json->file
    (case-lambda
      [(json file) (json->file json file #f)]
      [(json file pretty)
       (string->file (scm->json-string json #f pretty) file)]))
  )
