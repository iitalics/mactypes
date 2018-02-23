#lang racket/base
(require syntax/parse
         racket/match
         (only-in racket/string string-join))

(provide
 ;; Any -> Boolean
 ;; Returns #t if the argument is a type.
 type?

 ;; Id -> ConcreteType
 ;; Id [Listof Type] -> ConcreteType
 ;; Constructs a concrete type. The identifier must refer to a phase-0
 ;; macro bound to a MetaType value. Optionally takes a list of types
 ;; to use as arguments to the type.
 make-concrete-type

 concrete-type
 concrete-type?
 concrete-type-id
 concrete-type-arguments

 ;; Type -> String
 ;; Formats the type for use in UI such as error messages, REPL, etc.
 type->str

 ;; Type Type -> Boolean
 ;; Returns #t if the types are structurally the same.
 type=?

 ;; ================

 ;; ConcreteType -> MetaType
 ;; Extracts the meta type of the given concrete type.  A meta-type
 ;; describes the behavior of types derived by the meta-type The
 ;; meta-type decides functionality such as formatting, variance, etc.
 meta-type-of

 ;; The property value for prop:meta-type should be a list with a
 ;; single element: a function [MetaType Type -> String] for pretty-
 ;; printing types derived by this meta-type.
 prop:meta-type
 meta-type?
 meta-type-formatter

 ;; MetaType and [Stx -> Stx]
 ;; Meta-type that can be used for argument-less base types. The
 ;; meta-type also acts as a syntax syntax transformer which can be
 ;; evaluated to obtain a concrete type.
 base-type

 ;; MetaType and [Stx -> Stx]
 ;; Meta-type that can be used for function types, e.g. which accept
 ;; arguments and a return type. The meta-type also acts as a syntax
 ;; syntax transformer which can be evaluated to obtain a concrete
 ;; type.
 arrow-type

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

 ;; Syntax class which evaluates the syntax to obtain a type, and
 ;; reject non-type syntax. Attribute 'type' is that resulting Type.
 type-expr

 ;; Type -> Stx
 ;; Converts a type into a syntax object which will evaluate to that
 ;; type when passed to eval-syntax.
 ;; NOTE: this is used to send a type to a phase-1 template to be
 ;; later evaluated (by Racket, **not** by eval-type!!)
 type->expression

 )

;; ---------------------------------------------------------------

;;; types

(struct type [] #:prefab)

(struct concrete-type type [id arguments] #:prefab
  #:constructor-name mk-concrete-type)

(define (make-concrete-type id [args '()])
  (mk-concrete-type id args))

;;; meta types

(define (meta-type-of ct)
  (syntax-local-value (concrete-type-id ct)))

(define-values [prop:meta-type meta-type? meta-type-value]
  (make-struct-type-property 'meta-type))

(define (meta-type-formatter mt)
  (car (meta-type-value mt)))

;;; type utils

(define (type=? t1 t2)
  (match* [t1 t2]
    [[(concrete-type id1 args1)
      (concrete-type id2 args2)]
     (and (free-identifier=? id1 id2)
          (andmap type=? args1 args2))]
    [[_ _] #f]))

(define (type->str t)
  (match t
    [(concrete-type id args)
     ((meta-type-formatter (meta-type-of t)) t)]
    [_
     (raise-argument-error 'type->str "type?" t)]))

;;; type evaluation or synthesis

(define (type->expression t)
  (match t
    [(concrete-type id args)
     (with-syntax ([X id] [[arg ...] (map type->expression args)])
     #`(make-prefab-struct '(concrete-type type 0)
                           (quote-syntax X)
                           (list arg ...)))]
    [_
     (raise-argument-error 'type->str "type?" t)]))


(module typequote racket/base
  (provide (all-defined-out))
  (require (for-syntax racket/base))
  (define-syntax (typequote stx)
    (raise-syntax-error #f "invalid use of type outside of eval-type" stx)))

(require (for-template 'typequote))

(define (type->stx t [src #'t])
  (quasisyntax/loc src
    (typequote #,t)))

;; Stx -> Type
(define (datum-stx->type stx)
  (match (syntax-e stx)
    [(concrete-type id args)
     (let ([args/tys (map datum-stx->type (syntax->list args))])
       (if (memq #f args/tys) #f
           (mk-concrete-type id args/tys)))]
    [_ #f]))

(define (eval-type stx
                   [failure-result
                    (λ (orig elab)
                      (raise-syntax-error #f "invalid type expression" orig))])
  (syntax-parse (local-expand stx 'expression (list #'typequote))
    [((~literal typequote) datum)
     #:do [(define ty (datum-stx->type #'datum))]
     #:when ty ty]
    [stx+ (failure-result stx #'stx+)]))

(define-syntax-class type-expr
  #:attributes (type)
  [pattern e:expr
           #:attr type (eval-type #'e (λ _ #f))
           #:when (attribute type)])

;;; useful meta-types

(struct base-meta-type []
  #:property prop:procedure
  (λ (_ stx)
    (syntax-parse stx
      [X:id (type->stx (mk-concrete-type #'X '()))]
      [(X . _) (raise-syntax-error #f "type does not expect arguments" stx)]))

  #:property prop:meta-type
  (list (λ (ct) ;; formatter
          (symbol->string
           (syntax-e
            (concrete-type-id ct))))))
(define base-type (base-meta-type))


(struct arrow-meta-type []
  #:property prop:procedure
  (λ (_ stx)
    (syntax-parse stx
      [(==> arg ... ret ~!)
       #:do [(define args
               (map eval-type
                    (syntax->list #'[arg ... ret])))]
       (type->stx (mk-concrete-type #'==> args))]))

  #:property prop:meta-type
  (list (λ (ct) ;; formatter
          (match-define (concrete-type id args) ct)
          (format "[~a ~a]"
                  (syntax-e id)
                  (string-join (map type->str args))))))
(define arrow-type (arrow-meta-type))
