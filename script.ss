#!chezscheme

(library
  (script)
  (export println

          string-replace
          string-replace/all
          string-split
          string-join
          string->file
          file->string

          json-ref
          ss->json-file
          json-file->ss
          ss->json
          json->ss

          loge
          logw
          logi
          logd
          tab)

  (import (chezscheme) (strings) (ejson) (xtool))

  (define (println f . args)
    (apply printf (format "~a\n" f) args)
    )

  )
