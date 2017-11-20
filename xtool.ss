#!chezscheme
(library (xtool)
  (export xwarn
	  xinfo
	  xerror
	  xdebug
	  xintent
	  xshell)

  (import (chezscheme))
  
  (define _fun_
    (lambda (tag color)
      (lambda (msg . args)
	(apply printf (format "\033[3~am~a: ~a \033[0m\n" color tag msg) args))))

  (define xwarn ; 3 is yellow
    (_fun_ "Warn" 3))

  (define xinfo ; 2 is green
    (_fun_ "Info" 2))

  (define xerror ; 1 is red
    (_fun_ "Error" 1))

  (define xdebug  ; 6 is sky blue
    (_fun_ "Debug" 6))

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
