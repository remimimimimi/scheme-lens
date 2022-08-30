;; (library (lens)
;;  (export)
;;  (import (rnrs)))

(library-directories "./thunderchez")

(import (rnrs))
(import (srfi s1 lists))

(define (split-into-chunks n xs)
  (if (null? xs)
      '()
      (let ((first-chunk (take xs n))
            (rest (drop xs n)))
        (cons first-chunk (split-into-chunks n rest)))))

(define (immutable-set copy-fn set-fn!)
  (lambda (value target)
    (define value-copy (copy-fn value))
    (set-fn! value-copy target)
    value-copy))

;;; Lens related functionality
(define-structure (lens getter setter))

(define (lens-view lens target)
  ((lens-getter lens) target))

(define (lens-set lens target new-view)
  ((lens-setter lens) target new-view))

(define (lens-set/list target . lens-and-new-values)
  (fold-left
   (lambda (value lens-and-new-value)
     (define lens (car lens-and-new-value))
     (define new-value (cadr lens-and-new-value))
     (lens-set lens value new-value))
   target (split-into-chunks 2 lens-and-new-values)))


;;; Lenses
(define identity-lens
  (make-lens (lambda (x) x) (lambda (x value) value)))

(define car-lens
  (make-lens car (immutable-set list-copy set-car!)))

(define cdr-lens
  (make-lens cdr (immutable-set list-copy set-cdr!)))
