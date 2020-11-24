(define (print-stack-trace e)

  (define (get-func c)
      (let ((cc ((c 'code) 'name)))
        (if cc cc "--main--")))

  (display-condition e) (newline)

  (let p ((t (inspect/object (condition-continuation e))))
    (call/cc
     (lambda (ret)
       (if (> (t 'depth) 1)
           (begin
            (call-with-values
             (lambda () (t 'source-path))
             (case-lambda 
              ((file line column)
               (printf "\tat ~a (~a:~a,~a)\n" (get-func t) file line column))
              (else (ret))))
            (p (t 'link)))))))
  (exit))

(base-exception-handler print-stack-trace)
