(library 
  (csv)
  (export
    file->csv
    csv->file)
  (import (chezscheme) (script))

  (define (file->csv f)
    (define (split-symbols ls)
      (if (or (null? ls)
              (eq? #\return (car ls))
              (eq? #\newline (car ls)))
        '()
        (if (eq? #\, (car ls))
          (cons "" (split-symbols (cdr ls)))
          (let loop ([s (cons (car ls) '())]
                     [ls ls])
            (if (or (null? (cdr ls))
                    (eq? #\, (cadr ls))
                    (eq? #\return (cadr ls))
                    (eq? #\newline (cadr ls)))
              (cons (list->string (reverse s)) 
                    (if (null? (cdr ls))
                      '()
                      (split-symbols (cddr ls))))
              (loop (cons (cadr ls) s) (cdr ls)))))))
    (call-with-input-file
      f
      (lambda (p)
        (let loop ([s (get-line p)]
                   [r '()])
          (if (not (eof? s))
            (let ([l (string->list s)])
              (loop (get-line p) (append r `(,(split-symbols l)))))
            r)))))

  (define (csv->file c)
    #f)
)
