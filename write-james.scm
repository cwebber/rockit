;; JAMES: Javascript As Middling Expression of Sexprs

(define-module (rockit james write-js)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (write-james->jessie
            james->jessie-str))

(define test-mod
  '(module
    ;; #:name "@agoric/SwingSet/storageAPI"
    (import (%d assert
                (details #:as X))
            #:from "@agoric/assert")
    (defn (insistStorageAPI kvStore)
      #:export
      (for (const n of #("has" "getKeys" "get" "set" "delete"))
           (assert (in n kvStore)
                   (X (.concat "" "kvStore." n "is missing, cannot use")))))

    (defn (insistEnhancedStorageAPI kvStore)
      #:export
      (insistStorageAPI kvStore)
      (for (const n of #("enumeratePrefixedKeys"
                         "getPrefixedValues"
                         "deletePrefixedKeys"))
           (assert (in n kvStore)
                   (X (.concat "" "kvStore." n "is missing, cannot use")))))))


;; Things that aren't allowed to be variable names
(define jessie-special-terms
  '(null
    false true async arguments eval get set

    break else new var case finally return
    void catch for switch while continue
    function this with default if throw
    delete in try do instanceof typeof))

(define james-extra-special-terms
  '(fn
    defn begin))

(define james-special-terms
  (append jessie-special-terms james-extra-special-terms))

(define (james->jessie-str james-expr)
  (call-with-output-string
    (lambda (op)
      (write-james->jessie james-expr op))))

(define (dot-symbol? obj)
  (and (symbol? obj)
       (eq? (string-ref (symbol->string obj) 0) #\.)))

(define id-rx (make-regexp "^[$A-Za-z_][$A-Za-z0-9_]*$"))
(define (valid-id? obj)
  (and (symbol? obj)
       (regexp-exec id-rx (symbol->string obj))
       (not (member obj jessie-special-terms))))
;;;; qwik tests:
;; (valid-id? "foo") => #f
;; (valid-id? 'foo)  => #t
;; (valid-id? '9foo) => #f
;; (valid-id? 'if)   => #f

(define _void (if #f #f))

(define (write-james->jessie james-expr op)
  ;; display to out port
  (define (dop str)
    (display str op))
  (define (for-each-sep f l sep)
    (let lp ([l l]
             [first? #t])
      (match l
        ['() _void]
        [(item rest-l ...)
         (unless first?
           (dop sep))
         (f item)
         (lp rest-l #f)])))
  (define (write-top expr)
    (match expr
      [(or ('module top-exprs ...)
           (top-exprs ...))
       (for-each write-top-statement top-exprs)]))
  (define (write-top-statement expr)
    (match expr
      [('import _ ...)
       (write-import expr)]
      [('export _ ...)
       (write-export expr)]
      [('defn _ ...)
       (write-named-function expr #:top? #t)]
      [_ (write-statement expr)])
    (newline op))

  (define (write-block block-exprs)
    (for-each-sep write-statement block-exprs "\n"))

  (define (write-statement expr)
    (define (gather-elif-middle elif-middle)
      (match elif-middle
        ['() '()]
        [(#(condition body ...)
          rest ...)
         (cons (list condition body)
               (gather-elif-middle rest))]))
    (match expr
      ;; IF statements
      [(or ('if conditional arm else-arm)
           ('cond (conditional arm ...)
                  ('else else-arm ...)))
       (write-if conditional arm
                 #f else-arm)]
      [(or ('if conditional arm)
           ('cond ((and (not 'else)
                        conditional)
                   arm ...)))
       (write-if conditional arm #f #f)]
      [('cond
        (conditional arm ...)
        elif-middle ...
        ('else else-arm ...))
       (write-if conditional arm
                 (gather-elif-middle elif-middle)
                 else-arm)]
      ;; breakables: for, while, switch
      [('for _ ...) (write-for expr)]
      [('while _ ...) (write-while expr)]
      [('switch _ ...) (write-switch expr)]
      ;; terminator
      [('continue)
       (dop "continue;")]
      [('continue label-id)
       (dop "continue ")
       (write-id label-id)
       (dop ";")]
      [('break)
       (dop "break;")]
      [('break label-id)
       (dop "break ")
       (write-id label-id)
       (dop ";")]
      [('return) (dop "return;")]
      [('return expr)
       (dop "return ")
       (write-expr expr)
       (dop ";")]
      [('throw expr)
       (dop "throw ")
       (write-expr expr)
       (dop ";")]
      ;; labeled statements
      [(': label-id statement)
       (write-id label-id)
       (dop ": ")
       (write-statement statement)]
      ;; TRY block catcher finalizer
      [('try block catch-patterns ... #:finally finally-block)
       (dop "try {")
       (write-block block)
       (dop "} ")
       (for-each write-catch-pattern catch-patterns)
       (dop " finally {")
       (write-block finally-block)
       (dop "} ")]
      [('try block catch-pattern1 catch-patterns ...)
       (dop "try {")
       (write-block block)
       (dop "} ")
       (write-catch-pattern catch-pattern1)
       (for-each write-catch-pattern catch-patterns)]
      ;; DEBUGGER SEMI
      ;;; TODO
      ;; Allow for scheme-style begin statements
      [('begin body ...)
       (dop "(")
       (write-block body)
       (dop ") ")]
      ;; exprStatement;
      [expr (write-expr expr)
            (dop ";")]))

  (define (write-if conditional-expr arm
                    elifs else-arm)
    (dop "if (")
    (write-expr conditional-expr)
    (dop ") {")
    (write-statement arm)
    (dop "}")
    (when elifs
      (let lp ([elif elifs])
        (match elif
          [((elif-conditional elif-arm) rest ...)
           (dop " else if (")
           (write-expr elif-conditional)
           (dop ") {")
           (write-statement elif-arm)
           (dop "} ")
           (lp rest)]
          ['() 'done])))
    (when else-arm
      (dop " else {")
      (write-statement else-arm)
      (dop "}")))

  (define (write-array items)
    (dop "[")
    (for-each-sep write-expr items ", ")
    (dop "]"))

  (define (write-expr expr)
    (match expr
      ['undefined (dop "undefined")]
      ['null (dop "null")]
      [(or 'true #t) (dop "true")]
      [(or 'false #f) (dop "false")]
      [(or 'void #f) (dop "void")]
      [(? string?) (write-string expr)]
      [(? number?) (write-number expr)]
      [#(array-items ...)
       (write-array array-items)]
      ;; The %r means curly syntax
      ;; As an expression, that translates to a record
      [('%r propdefs ...)
       (write-reclike propdefs)]
      [('defn _ ...)
       (write-named-function expr)]
      [((or 'fn 'function 'λ) ...)
       (write-function expr)]

      ))

  ;;   FOR LEFT_PAREN declOp forOfBinding OF expr RIGHT_PAREN arm
  ;;   FOR LEFT_PAREN declaration expr? SEMI expr? RIGHT_PAREN arm
  (define (write-for expr)
    (error 'TODO)
    #;(match expr
      [('for bindings body ...)
       (display "for (" op)
       (write-for-bindings bindings)
       (display ") {" op)
       (write-expr)
       ]
      )
    )

  (define (write-id expr)
    (unless (valid-id? expr)
      (error "Not a valid id" expr))
    (dop (symbol->string expr)))

  ;;   WHILE LEFT_PAREN expr RIGHT_PAREN arm
  (define (write-while expr)
    (error 'TODO))

  ;;   SWITCH LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE clause* RIGHT_BRACE
  (define (write-switch expr)
    (error 'TODO))

  (define (write-import expr)
    (error 'TODO))

  (define (write-export expr)
    (error 'TODO))

  (define (write-string expr)
    ;; TODO: This probably doesn't handle all unicode stuff,
    ;; but it's the fastest hacky way...
    (format op "~s" expr))

  (define* (write-named-function expr #:key [top? #f])
    ;; TODO async, etc
    (match expr
      [(_ ((? valid-id? func-id) params ...)
          body ...)
       (dop "function ")
       (dop (symbol->string func-id))
       (dop " (")
       (write-func-params params)
       (dop ") {")
       (write-block body)
       (dop "}")]))

  (define (write-function expr)
    (match expr
      [(_ (params ...) body ...)
       (dop "function (")
       (write-func-params params)
       (dop ") {")
       (write-block body)
       (dop "} ")]))

  (define (write-func-params params)
    (define (write-param param)
      (match param
        [(? valid-id?) (write-id param)]
        ;; TODO: more coming soon
        ))
    (for-each-sep write-param params ", "))

  (define (write-reclike expr)
    (define (write-propdef propdef)
      (match propdef
        ;; TODO: more coming soon
        [(': (? valid-id? key) val)
         (dop (symbol->string key))
         (dop ": ")
         (write-expr expr)]))
    (for-each-sep write-propdef expr ", "))

  (define (write-catch-pattern expr)
    (error 'TODO))

  ;; @@: We never write exponents... that's ok, right?
  (define (write-number expr)
    (match expr
      [(or (? integer?) (? inexact?)) (dop (number->string expr))]))

  (write-top james-expr))
