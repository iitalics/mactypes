#lang racket/base
(require (for-syntax racket))

(define A- 0)
(define B- 0)

(begin-for-syntax
  (require mactype/types
           rackunit)

  (define (not-type<:? a b) (not (type<:? a b)))

  (define-binary-check (check-<: type<:? lhs-type rhs-type))
  (define-binary-check (check-not-<: not-type<:? lhs-type rhs-type))

  (define A (make-base-type 'A #'A-))
  (define B (make-base-type 'B #'B-))

  (check-<: A A)
  (check-<: A (make-base-type 'C #'A-))
  (check-not-<: A B)

  (check-eq? 'foobar
             (eval-type #'im-not-a-type
                        (λ (orig elab)
                          'foobar)))

  (define A-stx-1 #`(typequote #,A))
  (define A-stx-2 (type->stx A))

  ;; NOTE: these tests fail because syntax->datum traverses the prefab
  ;;   struct; therefore our identifier field is turned back into a
  ;;   symbol! syntax-e is unfortunately not an easy solution, since
  ;;   it converts too little: the other symbol field will wind up
  ;;   being syntax objects when they don't intend to be.
  (check-<: A (eval-type A-stx-1))
  (check-<: A (eval-type A-stx-2))

  )
