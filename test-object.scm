(import (object))

(define a (obj))

(define b (obj))

(printf "~a\n" (eq? a b))

(printf "~a\n" (obj-has? a 'ok))

(printf "~a\n" (obj-has? a 'yes))

(obj-set! a 'ok '(we are family))

(obj-set! b 'yes '(good day))

(obj-set! a 'yes (obj))

(obj-set! (obj-ref a 'yes) 'yes '(i am yes field))

(obj-set! a 'array (array))

(array-push (obj-ref a 'array) '(we are good day))

(array-push (obj-ref a 'array) '(yes we are))
(array-push (obj-ref a 'array) '(no no no yes we are))

(printf "~a\n ~a\n" a b)

(array-out (obj-ref a 'array))
(printf "~a\n" a)

(array-pop (obj-ref a 'array))
(printf "~a\n" a)

(array-in (obj-ref a 'array) '(i am in))
(printf "~a\n" a)

(obj-rm! a 'array)

(array-pop (obj-ref a 'array))
(printf "~a\n" a)

