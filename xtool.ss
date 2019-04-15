#!chezscheme

(library
  (xtool)
  (export logw
          logi
          loge
          logd
          tab)

  (import (chezscheme))

  (define _fun_
    (lambda (tag color)
      (lambda (msg . args)
        (apply printf (format "~a~a: ~a\033[0m\n" color tag msg) args))))

  (define logw
    (_fun_ "W" "\033[1;33m"))

  (define logi
    (_fun_ "I" "\033[0m"))

  (define loge
    (_fun_ "E" "\033[1;31m"))

  (define logd
    (_fun_ "D" "\033[1;37m"))

  (define tab
    (lambda (segment msg . args)
      (apply printf (format (format "~~,,~a@a\n" (* 4 segment)) msg) args)))
  )