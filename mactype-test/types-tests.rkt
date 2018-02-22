#lang racket/base
(require
 (for-syntax racket
             syntax/parse
             mactype/types
             rackunit))

(define-syntax type-info:A (make-type-info 'A))
(define-syntax type-info:B (make-type-info 'B #:format (λ _ "B")))
(define-syntax type-info:A- (make-rename-transformer #'type-info:A))

(begin-for-syntax

  (define-binary-check (check-type=? type=? actual expected))
  (define-binary-check (check-not-type=? (negate type=?) actual expected))

  (define A (make-concrete-type #'type-info:A))
  (define B (make-concrete-type #'type-info:B))

  (check-type=? A A)
  (check-type=? A (make-concrete-type #'type-info:A-))
  (check-not-type=? A B)

  (check-equal? (type->str A) "(A)")
  (check-equal? (type->str B) "B")

  ;; -----------

  (define A-stx (type->stx A))
  (check-type=? A (eval-type A-stx))

  (check-exn exn:fail? (λ () (eval-type #'im-not-a-type)))
  (check-equal? 'foobar (eval-type #'im-not-a-type (λ (a b) 'foobar)))

  )
