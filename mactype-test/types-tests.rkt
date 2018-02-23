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

  (define-binary-check (check-type=? type=? actual expected))
  (define-binary-check (check-not-type=? (negate type=?) actual expected))

  (define a (eval-type #'A))
  (define b (eval-type #'B))
  (define a->b (eval-type #'[→ A B]))

  (check-equal? (meta-type-of a) (syntax-local-value #'A))
  (check-type=? a a)
  (check-not-type=? a b)

  (check-equal? (type->str a) "A")
  (check-equal? (type->str a->b) "[→ A B]")

  (check-exn exn:fail:syntax? (λ () (eval-type #'(A 1 2 3))))
  (check-exn exn:fail:syntax? (λ () (eval-type #'→)))
  (check-exn exn:fail? (λ () (eval-type #'im-not-a-type)))

  )
