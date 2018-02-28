#!chezscheme

(library
  (script)
  (export println
          ls

          string-replace
          string-replace/all
          string-split
          string-join
          string->file
          file->string
          file->utf8-iport
          file->utf8-oport
          string->iport
          string->oport

          json-ref
          ss->json-file
          json-file->ss
          ss->json
          json->ss

          loge
          logw
          logi
          logd
          tab

          match
          match-lambda
          match-lambda*
          match-let
          match-let*
          match-letrec
          match-named-let

          xml->ss
          xml-ref
          )

  (import (chezscheme) (strings) (ejson) (xtool) (matchable) (sxml))

  (define (println f . args)
    (apply printf (format "~a\n" f) args)
    )

  (define (file->utf8-iport f)
    (open-file-input-port f (file-options) 'block (make-transcoder (utf-8-codec))))

  (define (file->utf8-oport f)
    (open-file-output-port f (file-options) 'block (make-transcoder (utf-8-codec))))

  (define-syntax string->iport (identifier-syntax open-string-input-port))
  (define-syntax string->oport (identifier-syntax open-string-output-port))
  (define-syntax ls (identifier-syntax directory-list))

  (define xml->ss
    (case-lambda
      [(f) (xml->ss f '())]
      [(f ns) (ssax:xml->sxml (file->utf8-iport f) ns)]))

  (define (xml-ref xml path)
    ((sxpath path) xml))
  )
