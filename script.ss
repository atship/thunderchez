#!chezscheme

(library
  (script)
  (export println
          ls
          shell

          string-replace
          string-replace/all
          string-split
          string-join
          string->file
          file->string
          file->utf8-iport
          file->utf8-oport
          file->iport
          file->oport
	        file-iclose
	        file-oclose
          string->iport
          string->oport

          json-ref
          json->file
          file->json

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

          read-bytes
          write-bytes
          read-int
          read-uint
          read-long
          read-ulong
          read-short
          read-ushort
          read-byte
          read-ubyte
          read-utf8
          read-float
          read-double
          write-utf8
          write-uint
          write-int
          write-ulong
          write-long
          write-ushort
          write-short
          write-ubyte
          write-byte
          write-float
          write-double

          xml->ss
          xml-ref
          )

  (import (chezscheme) (strings) (ejson) (xtool) (matchable) (sxml))

  (define (shell f . args)
    (system (apply format f args)))

  (define (println f . args)
    (apply printf (format "~a\n" f) args))

  (define (file->utf8-iport f)
    (open-file-input-port f (file-options) 'block (make-transcoder (utf-8-codec))))

  (define (file->utf8-oport f)
    (open-file-output-port f (file-options replace) 'block (make-transcoder (utf-8-codec))))

  (define-syntax file->iport (identifier-syntax open-file-input-port))

  (define (file->oport f)
    (open-file-output-port f (file-options replace) 'block))

  (define-syntax file-iclose (identifier-syntax close-input-port))

  (define-syntax file-oclose (identifier-syntax close-output-port))
  
  (define-syntax string->iport (identifier-syntax open-string-input-port))

  (define-syntax string->oport (identifier-syntax open-string-output-port))

  (define-syntax ls (identifier-syntax directory-list))

  (define-syntax read-bytes (identifier-syntax get-bytevector-n))

  (define-syntax write-bytes (identifier-syntax put-bytevector))

  (define (read-int port)
    (bytevector-s32-ref (read-bytes port 4) 0 'little))

  (define (read-uint port)
    (bytevector-u32-ref (read-bytes port 4) 0 'little))

  (define (read-long port)
    (bytevector-s64-ref (read-bytes port 8) 0 'little))

  (define (read-ulong port)
    (bytevector-u64-ref (read-bytes port 8) 0 'little))

  (define (read-short port)
    (bytevector-s16-ref (read-bytes port 2) 0 'little))

  (define (read-ushort port)
    (bytevector-u16-ref (read-bytes port 2) 0 'little))

  (define (read-byte port)
    (bytevector-s8-ref (read-bytes port 1) 0))

  (define (read-ubyte port)
    (bytevector-u8-ref (read-bytes port 1) 0))

  (define (read-float port)
    (bytevector-ieee-single-ref (read-bytes port 4) 0 'little))

  (define (read-double port)
    (bytevector-ieee-double-ref (read-bytes port 8) 0 'little))

  (define (write-uint port i)
    (let ([bytes (make-bytevector 4)])
      (bytevector-u32-set! bytes 0 i 'little)
      (write-bytes port bytes)))

  (define (write-int port i)
    (let ([bytes (make-bytevector 4)])
      (bytevector-s32-set! bytes 0 i 'little)
      (write-bytes port bytes)))

  (define (write-ulong port l)
    (let ([bytes (make-bytevector 8)])
      (bytevector-u64-set! bytes 0 l 'little)
      (write-bytes port bytes)))

  (define (write-long port l)
    (let ([bytes (make-bytevector 8)])
      (bytevector-s64-set! bytes 0 l 'little)
      (write-bytes port bytes)))

  (define (write-short port s)
    (let ([bytes (make-bytevector 2)])
      (bytevector-s16-set! bytes 0 s 'little)
      (write-bytes port bytes)))

  (define (write-ushort port s)
    (let ([bytes (make-bytevector 2)])
      (bytevector-u16-set! bytes 0 s 'little)
      (write-bytes port bytes)))

  (define (write-byte port b)
    (let ([bytes (make-bytevector 1)])
      (bytevector-s8-set! bytes 0 b)
      (write-bytes port bytes)))

  (define (write-ubyte port b)
    (let ([bytes (make-bytevector 1)])
      (bytevector-u8-set! bytes 0 b)
      (write-bytes port bytes)))

  (define (write-float port f)
    (let ([bytes (make-bytevector 4)])
      (bytevector-ieee-single-set! bytes 0 f 'little)
      (write-bytes port bytes)))

  (define (write-double port f)
    (let ([bytes (make-bytevector 8)])
      (bytevector-ieee-double-set! bytes 0 f 'little)
      (write-bytes port bytes)))
  
  (define read-utf8
    (case-lambda
      [(port) (let ([len (read-uint port)]) (read-utf8 port len))]
      [(port len) (utf8->string (read-bytes port len))]))

  (define (write-utf8 port s)
    (let ([bytes (string->utf8 s)])
      (write-uint port (bytevector-length bytes))
      (write-bytes port bytes)))

  (define xml->ss
    (case-lambda
      [(f) (xml->ss f '())]
      [(f ns) (ssax:xml->sxml (file->utf8-iport f) ns)]))

  (define (xml-ref xml path)
    ((sxpath path) xml))

  )
