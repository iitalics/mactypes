#lang racket/base
(require syntax/parse
         racket/match)

(provide
 ;; Any -> Boolean
 ;; Returns #t if the argument is a type.
 type?

 ;; Id -> ConcreteType
 ;; Id [Listof Type] -> ConcreteType
 ;; Constructs a concrete type. The identifier must refer to a phase-0
 ;; macro bound to a TypeInfo value. Optionally takes a list of
 ;; arguments to use as arguments to the type.
 make-concrete-type

 concrete-type
 concrete-type?
 concrete-type-id
 concrete-type-arguments

 ;; Symbol {#:format [ConcreteType -> String]} -> TypeInfo
 make-type-info

 type-info
 type-info?

 ;; ConcreteType -> TypeInfo
 ;; Extracts the type info out of the given type. Equivalent to:
 ;;   (compose syntax-local-value concrete-type-id)
 concrete-type-info

 ;; Type -> String
 ;; Formats the type for use in UI such as error messages, REPL, etc.
 type->str

 ;; Type Type -> Boolean
 ;; Returns #t if the types are structurally the same.
 type=?

 ;; ================

 ;; Stx -> Type
 ;; Stx [Stx Stx -> Any] -> Any
 ;; "Evaluates" the given syntax object to obtain a Type value. If the
 ;; given syntax does not expand to a valid type representation then
 ;; eval-type either raises a syntax error, or calls the given
 ;; function with two arguments: the given syntax object and the macro
 ;; expansion of that syntax object.
 eval-type

 ;; Type [Stx] -> Stx
 ;; Converts a type into a syntax object which will evaluate to that
 ;; type when passed to eval-type. Takes an optional additional
 ;; argument to use as the source location for the returned syntax
 ;; object.
 type->stx
 (for-template typequote)

 ;; Type -> Stx
 ;; Converts a type into a syntax object which will evaluate to that
 ;; type when passed to eval-syntax.
 ;; NOTE: this is used to send a type to a phase-1 template to be
 ;; later evaluated (by Racket, **not** by eval-type!!)
 ;type->expression

 ;; Type -> [Stx -> Stx]
 ;; Returns a syntax transformer that transforms identifiers into a
 ;; representation for the given type (to be used by eval-type).
 ;;   (define-syntax Int (id-type-transformer my-int-type))
 ;;   (begin-for-syntax
 ;;     ... (type<:? (eval-type #'Int) my-int-type)) ; -> #t
 id-type-transformer

 )

;; ---------------------------------------------------------------

(struct type [] #:prefab)
(struct concrete-type type [id arguments] #:prefab
  #:constructor-name mk-concrete-type)

(define (make-concrete-type id [args '()])
  (mk-concrete-type id args))

(define (concrete-type-info ct)
  (syntax-local-value (concrete-type-id ct)))

(struct type-info [name formatter subtype]
  #:constructor-name mk-type-info)

(define (type->str t)
  (cond
    [(concrete-type? t)
     ((type-info-formatter (concrete-type-info t)) t)]
    [else (raise-argument-error 'type->str "type?" t)]))

(define (default-type-format t)
  (define args (concrete-type-arguments t))
  (define info (concrete-type-info t))
  (format "~a"
          (cons (type-info-name info)
                (map type->str args))))

(define (make-type-info name
                        #:format [fmt default-type-format])
  (mk-type-info name fmt #f))

(define (type=? t1 t2)
  (match* [t1 t2]
    [[(concrete-type id1 args1)
      (concrete-type id2 args2)]
     (and (free-identifier=? id1 id2)
          (= (length args1) (length args2))
          (andmap type=? args1 args2))]
    [[_ _] #f]))

;; ================

(module typequote racket/base
  (provide (all-defined-out))
  (require (for-syntax racket/base))
  (define-syntax (typequote stx)
    (raise-syntax-error #f "typequote may not be used outside of eval-type" stx)))
(require (for-template 'typequote))

(define (e->type e)
  (match e
    [(concrete-type id args)
     (mk-concrete-type id (map e->type (syntax->list args)))]
    [_ #f]))

(define (eval-type e
                   [failure-result
                    (λ (orig _)
                      (raise-syntax-error #f "invalid type expression" orig))])
  (syntax-parse (local-expand e 'expression (list #'typequote))
    [((~literal typequote) datum)
     #:do [(define ty (e->type (syntax-e #'datum)))]
     #:when ty ty]
    [e+ (failure-result e #'e+)]))

(define (type->stx t [src #'_])
  (quasisyntax/loc src
    (typequote #,t)))

(define (id-type-transformer t)
  (syntax-parser
    [x:id (type->stx t #'x)]
    [e (raise-syntax-error #f "must be used as identifier" this-syntax #'e)]))
