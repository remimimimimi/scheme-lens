;; (library (lens)
;;  (export)
;;  (import (rnrs)))

(library-directories "./thunderchez")

(import (rnrs))
(import (srfi s1 lists))

;; Various utils
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

(define (list-ref-set lst n new-value)
  ;; (assert (or (< n 0) (>= n (length lst))))
  (define (list-ref-set-aux lst n)
    (if (= n 0)
        (cons new-value (cdr lst))
        (cons (car lst) (list-ref-set-aux (cdr lst) (- n 1)))))
  (list-ref-set-aux lst n))

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

;; Pair lenses
(define car-lens (make-lens car (immutable-set list-copy set-car!)))
(define cdr-lens (make-lens cdr (immutable-set list-copy set-cdr!)))
(define caar-lens (lens-compose car-lens car-lens))
(define cadr-lens (lens-compose car-lens cdr-lens))
(define cdar-lens (lens-compose cdr-lens car-lens))
(define cddr-lens (lens-compose cdr-lens cdr-lens))
(define caaar-lens (lens-compose car-lens caar-lens))
(define cdaar-lens (lens-compose cdr-lens caar-lens))
(define cadar-lens (lens-compose car-lens cdar-lens))
(define cddar-lens (lens-compose cdr-lens cdar-lens))
(define caadr-lens (lens-compose car-lens cadr-lens))
(define cdadr-lens (lens-compose cdr-lens cadr-lens))
(define caddr-lens (lens-compose car-lens cddr-lens))
(define cdddr-lens (lens-compose cdr-lens cddr-lens))

;; List lenses
(define (list-ref-lens n)
  (make-lens (lambda (lst) (list-ref lst n))
             (lambda (lst new-view) (list-ref-set lst n new-view))))
(define first-lens (list-ref-lens 0))
(define second-lens (list-ref-lens 1))
(define third-lens (list-ref-lens 2))
(define fourth-lens (list-ref-lens 3))
(define fifth-lens (list-ref-lens 4))
(define sixth-lens (list-ref-lens 5))
(define seventh-lens (list-ref-lens 6))
(define eighth-lens (list-ref-lens 7))
(define ninth-lens (list-ref-lens 8))
(define tenth-lens (list-ref-lens 9))

(define (list-ref-nested-lens . indices)
  (apply lens-compose (reverse (map list-ref-lens indices))))
