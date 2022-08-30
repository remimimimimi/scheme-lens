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
(define-record lens (getter setter))

(define (lens-view lens target)
  ((lens-getter lens) target))

(define (lens-view/list target . lenses)
  (map (lambda (lens) (lens-view lens target)) lenses))

(define (lens-set lens target new-view)
  ((lens-setter lens) target new-view))

(define (lens-set/list target . lens-and-new-values)
  (fold-left
   (lambda (value lens-and-new-value)
     (define lens (car lens-and-new-value))
     (define new-value (cadr lens-and-new-value))
     (lens-set lens value new-value))
   target (split-into-chunks 2 lens-and-new-values)))

(define (lens-transform lens target transformer)
  ((lens-setter lens) target (transformer (lens-view lens target))))

(define (lens-transform/list target . lens-and-fns)
  (fold-left
   (lambda (value lens-and-fn)
     (define lens (car lens-and-fn))
     (define fn (cadr lens-and-fn))
     (lens-transform lens value fn))
   target (split-into-chunks 2 lens-and-fns)))

;;; Lens utilities
(define (lens-compose2 sub-lens super-lens)
  (define (get target)
    (lens-view sub-lens (lens-view super-lens target)))
  (define (set target new-view)
    (define sub-view (lens-view super-lens target))
    (define new-sub-view (lens-set sub-lens sub-view new-view))
    (lens-set super-lens target new-sub-view))
  (make-lens get set))

(define (lens-compose . args)
  (fold-left lens-compose2 identity-lens args))

;;; Lenses
(define identity-lens
  (make-lens (lambda (x) x) (lambda (x value) value)))

(define car-lens
  (make-lens car (immutable-set list-copy set-car!)))

(define cdr-lens
  (make-lens cdr (immutable-set list-copy set-cdr!)))
