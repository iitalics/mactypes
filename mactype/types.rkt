#lang racket/base
(require syntax/parse
         racket/match)

(provide
 ;; Any -> Boolean
 ;; Returns #t if the argument is a type.
 type?

 ;; Symbol Id -> Type
 ;; Constructs a base type with the given display name and identifier.
 ;; The identifier is used with free-identifier=? to check for
 ;; equality.
 ;;
 ;; TODO: reconsider the use of identifers. OOH, it provides a pretty
 ;;   resonable way to check for equality, and more meta-info can be
 ;;   expressed through syntax-local-value. OTOH, I'm wary of scopes
 ;;   being easily messed up as it goes in and out of templates.
 make-base-type

 ;; Type Type -> Boolean
 ;; Subtyping relation for types; returns #t if the first argument is
 ;; more specific than the second.
 type<:?

 ;; Type -> String
 ;; Formats the type for use in UI such as error messages, REPL, etc.
 type->str

 ;; ================

 ;; Stx -> Type
 ;; Stx [Stx Stx -> Any] -> Any
 ;; "Evaluates" the given syntax object to obtain a type value. If the
 ;; given syntax does not expand to a valid type representation (see
 ;; typequote and type->str) then eval-type raises a syntax error.
 ;;
 ;; The syntax error can be avoided by passing an optional additional
 ;; argument, failure-result. When the expression fails to produce a
 ;; valid type, eval-type returns '(failure-result e-orig e-elab)'
 ;; where e-orig is the syntax object passed to eval-type and e-elab
 ;; is the macro expansion of that syntax object.
 eval-type

 ;; Type [Stx] -> Stx
 ;; Converts a type into a syntax object which will evaluate to that
 ;; type when passed to eval-type. Takes an optional additional
 ;; argument to use as the source locations for the returned syntax
 ;; object.
 type->stx

 ;; SYNTAX: (typequote type-datum)
 ;; typequote expressions are recognized by eval-type, which returns
 ;; the inner 'type-datum' when reached. typequote is not valid
 ;; anywhere else.
 (for-template typequote)

 )

;; ---------------------------------------------------------------

(struct type [] #:prefab)
(struct base-type type [name ident] #:prefab
  #:extra-constructor-name make-base-type)

(define (type<:? t u)
  (match* [t u]
    [[(base-type _ id-1)
      (base-type _ id-2)]
     (free-identifier=? id-1 id-2)]))

(define (type->str t)
  (match t
    [(base-type name _) (symbol->string name)]))

;; ================

(module typequote racket/base
  (provide (all-defined-out))
  (require (for-syntax racket/base))
  (define-syntax (typequote stx)
    (raise-syntax-error #f "typequote may not be used outside of eval-type" stx)))
(require (for-template 'typequote))

(define (type->stx t [src #'_])
  (quasisyntax/loc src
    (typequote #,t)))

(define (eval-type e
                   [failure-result
                    (λ (orig _)
                      (raise-syntax-error #f "invalid type expression" orig))])
  (syntax-parse (local-expand e 'expression (list #'typequote))
    [((~literal typequote) datum)
     #:do [(define ty (syntax->datum #'datum))]
     #:when (type? ty) ty]
    [e+ (failure-result e #'e+)]))
