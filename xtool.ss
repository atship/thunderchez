(library (xtool)
  (export xwarn
	  xinfo
	  xerror
	  xdebug
	  xintent
	  xshell)

  (import (chezscheme))
  
  (define _fun_
    (lambda (tag)
      (lambda (msg . args)
	(apply printf (format "~a: ~a\n" tag msg) args))))

  (define xwarn
    (_fun_ "Warn"))

  (define xinfo
    (_fun_ "Info"))

  (define xerror
    (_fun_ "Error"))

  (define xdebug
    (_fun_ "Debug"))

  (define xintent
    (lambda (segment msg . args)
      (apply printf (format (format "~~,,~a@a\n" (* 4 segment)) msg) args)))

  (define xshell-debug
    (lambda (cmd . args)
       (newline)))
  
  (define xshell
    (lambda (cmd . args)
      (if (top-level-bound? 'DEBUG)
	  (printf (apply format cmd args))
	  (system (apply format cmd args)))
      (newline))))
