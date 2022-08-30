;; (library (lens)
;;  (export)
;;  (import (rnrs)))

(import (rnrs))

;;; Helper functions
(define (take n xs)
  (let loop ((n n) (xs xs) (zs (list)))
    (if (or (zero? n) (null? xs))
        (reverse zs)
        (loop (- n 1) (cdr xs)
              (cons (car xs) zs)))))

(define (split-by n l)
  (let ((size (quotient (length l) n)))
    (apply chunk-list l (make-list size n))))

(define (split-by n l)
  (let ((size (quotient (length l) n)))
    (apply chunk-list l (make-list size n))))

(define (chunks n xs)
  (define (divisor? m n)
    (= 0 (remainder m n)))
  (if (divisor? (length xs) n)
      (begin
        (define len (/ (length x) n)))))


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

(define (lens-view/list target . lenses)
  (map (lambda (lens) (lens-view lens target)) lenses))

(define (lens-set/list target . lenses)
  (fold-left (lambda (value lens)) target lenses))

;;; Lenses
(define identity-lens
  (make-lens (lambda (x) x) (lambda (x value) value)))

(define car-lens
  (make-lens car (immutable-set list-copy set-car!)))

(define cdr-lens
  (make-lens cdr (immutable-set list-copy set-cdr!)))
