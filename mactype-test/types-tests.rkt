#lang racket/base
(require
 (for-syntax racket
             syntax/parse
             mactype/types
             rackunit))

(define-syntax A base-type)
(define-syntax B base-type)
(define-syntax → arrow-type)

(begin-for-syntax

  (define-binary-check (check-free-id=? free-identifier=? actual expected))
  (define-binary-check (check-type=? type=? actual expected))
  (define-binary-check (check-not-type=? (negate type=?) actual expected))

  (define a    (eval-type #'A))
  (define b    (eval-type #'B))
  (define a->b (eval-type #'[→ A B]))
  (define b->a (eval-type #'[→ B A]))

  (check-false (syntax? a))
  (check-true (type? a))

  (check-free-id=? (concrete-type-id a) #'A)
  (check-equal? (meta-type-of a) base-type)

  (check-type=?     a a)
  (check-not-type=? a b)
  (check-type=?     a->b a->b)
  (check-not-type=? b->a a->b)

  (check-equal? (type->str a)    "A")
  (check-equal? (type->str a->b) "[→ A B]")

  (check-exn #px"does not expect arguments" (λ () (eval-type #'(A 1 2 3))))
  (check-exn #px"invalid type expression" (λ () (eval-type #'im-not-a-type)))
  (check-equal? 'foo (eval-type #'im-not-a-type (λ _ 'foo)))

  )

;; NOTE: (eval-syntax (type->expression A)) does NOT work
;;   as intended, but this is likely because eval is just
;;   really hard to work with. nevertheless, the scopes seem
;;   to be correct (in some sense of the word), exemplified
;;   below:

(define-syntax phase-1-expr-test
  (syntax-parser
    [_ #:with mk-a (type->expression a)
       #'(begin-for-syntax
           (check-type=? mk-a a))]))

(phase-1-expr-test)
