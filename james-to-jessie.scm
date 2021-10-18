;; JAMES: Jessie As Middled Expression, Symbolically
;; (Remembering of course that Jessie is a subset of Javascript!)

(define-module (rockit james write-js)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (write-james->jessie
            james->jessie-str))

;; Things that aren't allowed to be variable names
(define jessie-special-terms
  '(null
    false true async arguments eval get set

    break else new var case finally return
    void catch for switch while continue
    function this with default if throw
    delete in try do instanceof typeof let))

(define james-extra-special-terms
  '(fn
    defn begin
    def

    ;; ones that aren't valid-id? anyway
    m.))


(define james-special-terms
  (append jessie-special-terms james-extra-special-terms))

(define (james->jessie-str james-expr)
  (call-with-output-string
    (lambda (op)
      (write-james->jessie james-expr op))))

(define dot-method-rx
  (make-regexp "^\\.[$A-Za-z_][$A-Za-z0-9_]*$"))
(define (dot-method? obj)
  (and (symbol? obj)
       (regexp-exec dot-method-rx
                    (symbol->string obj))))

(define id-rx (make-regexp "^[$A-Za-z_][$A-Za-z0-9_]*$"))
(define (valid-id? obj)
  (and (symbol? obj)
       (regexp-exec id-rx (symbol->string obj))
       (not (member obj jessie-special-terms))))


(define dot-method-combined-rx
  (make-regexp "^[$A-Za-z_][$A-Za-z0-9_]*\\.[$A-Za-z_][$A-Za-z0-9_]*$"))
(define (dot-method-combined-symbol? obj)
  (and (symbol? obj)
       (regexp-exec dot-method-combined-rx
                    (symbol->string obj))))
;;;; qwik tests:
;; (valid-id? "foo") => #f
;; (valid-id? 'foo)  => #t
;; (valid-id? '9foo) => #f
;; (valid-id? 'if)   => #f

(define _void (if #f #f))

(define (dot-method-expr? expr)
  (match expr
    [('m. method-of-expr (? valid-id?))
     #t]
    [_ #f]))

(define multi-infixer-ops
  '(+ - / *))
(define single-infixer-ops
  '(=== ==! % += -=))

(define (multi-infixer-op? obj)
  (member obj multi-infixer-ops))
(define (single-infixer-op? obj)
  (member obj single-infixer-ops))
(define (infixer-op? expr)
  (or (multi-infixer-op? expr)
      (single-infixer-op? expr)))

(define (write-james->jessie james-expr op)
  (define indent-level (make-parameter 0))
  (define-syntax-rule (up-indentation body ...)
    (parameterize ((indent-level (1+ (indent-level))))
      body ...))
  ;; display to out port
  (define (dop str)
    (display str op))
  (define (newline-indent)
    (newline op)
    (for-each (lambda _ (dop "  "))
              (iota (* (indent-level)))))
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
  ;; for each newline-indent
  (define* (for-each-nli f l #:key [sep #f])
    (let lp ([l l]
             [first? #t])
      (match l
        ['() _void]
        [(item rest-l ...)
         (when (and sep (not first?))
           (dop sep))
         (newline-indent)
         (f item)
         (lp rest-l #f)])))
  (define (write-top expr)
    (match expr
      [('module top-exprs ...)
       (for-each write-top-statement top-exprs)]
      [_ (write-top-statement expr)]))
  (define (write-top-statement expr)
    (match expr
      [('import _ ...)
       (write-import expr)]
      [('export _ ...)
       (write-export expr)]
      [('defn _ ...)
       (write-named-function expr #:top? #t)]
      [_ (write-statement expr)])
    (newline op) (newline op))
  (define (write-block block-exprs)
    (for-each-nli (lambda (exp)
                    (write-statement exp))
                  block-exprs))

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
       (write-statement statement)
       (dop ";")]
      ;; TRY block catcher finalizer
      [('try block catch-patterns ... #:finally finally-block)
       (dop "try {")
       (newline-indent)
       (up-indentation
        (write-block block))
       (newline-indent)
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
      [('set! (? valid-id? to-assign) val-expr)
       (dop (symbol->string to-assign))
       (dop " = ")
       (write-expr val-expr)
       (dop ";")]
      [('defconst (? valid-id? id) val-expr)
       (dop "const ")
       (dop (symbol->string id))
       (dop " = ")
       (write-expr val-expr)
       (dop ";")]
      [((or 'let 'def) (? valid-id? id) val-expr)
       (dop "let ")
       (dop (symbol->string id))
       (dop " = ")
       (write-expr val-expr)
       (dop ";")]
      ;; DEBUGGER SEMI
      ;;; TODO
      ;; Allow for scheme-style begin statements
      [('begin body ...)
       (dop "(")
       (up-indentation (write-block body))
       (newline-indent)
       (dop ")")]
      
      ;; exprStatement;
      [expr (write-expr expr)
            (dop ";")]))

  ;; Jessie is sensible and decides to reject "order of operation hell"
  ;; and so do we by wrapping all nested infixed things into parenthes
  (define (write-infix-safe-expr expr)
    (match expr
      [((? infixer-op?) rest ...)
       (dop "(")
       (write-expr expr)
       (dop ")")]
      [_ (write-expr expr)]))

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
      [((or 'fn 'function 'λ) _ ...)
       (write-function expr)]
      [((? single-infixer-op? infixer-op) arg1 arg2)
       (write-infix-safe-expr arg1)
       (dop " ")
       (dop (symbol->string infixer-op))
       (dop " ")
       (write-infix-safe-expr arg1)]
      [((? multi-infixer-op? infixer-op) arg1 arg-rest ...)
       (for-each-sep write-infix-safe-expr
                     (cons arg1 arg-rest)
                     (string-append " " (symbol->string infixer-op) " "))]

      [('.-> initial-expr dot-method-calls ...)
       (write-expr initial-expr)
       (let lp ((dot-method-calls dot-method-calls))
         (match dot-method-calls
           ['() _void]
           [(((? dot-method? dot-method) args ...) rest ...)
            (dop (symbol->string dot-method))
            (dop "(")
            (for-each-sep write-expr args ", ")
            (dop ")")
            (lp rest)]))]

      [('.m method-of-expr (? dot-method? dot-method))
       (write-expr method-of-expr)
       (dop (symbol->string dot-method))]

      ;; Otherwise... it's some other evaluated procedure defined by the first
      ;; argument's evaluated expression
      [(method-of-expr (? dot-method? dot-method) args ...)
       (write-expr expr)
       (dop (symbol->string dot-method))
       (dop "(")
       (for-each-sep write-expr args ", ")
       (dop ")")]

      ;; At this point we let whatever expression be the invocation
      [(to-call args ...)
       (write-expr to-call)
       (dop "(")
       (for-each-sep write-expr args ", ")
       (dop ")")]
      [(? dot-method-combined-symbol? dmc)
       (dop (symbol->string dmc))]
      [(? valid-id? id)
       (dop (symbol->string id))]))

  (define (write-if conditional-expr arm
                    elifs else-arm)
    (define (write-arm expr)
      (up-indentation
       (write-block
        (match expr
          [('begin body ...) body]
          [_ (list expr)])))
      (newline-indent))
    (dop "if (")
    (write-expr conditional-expr)
    (dop ") {")
    (write-arm arm)
    (dop "}")
    (when elifs
      (let lp ([elif elifs])
        (match elif
          [((elif-conditional elif-arm) rest ...)
           (dop " else if (")
           (write-expr elif-conditional)
           (dop ") {")
           (write-arm elif-arm)
           (dop "} ")
           (lp rest)]
          ['() 'done])))
    (when else-arm
      (dop " else {")
      (write-arm else-arm)
      (dop "}")))

  (define (write-array items)
    (dop "[")
    (for-each-sep write-expr items ", ")
    (dop "]"))

  ;;   FOR LEFT_PAREN declOp forOfBinding OF expr RIGHT_PAREN arm
  ;;   FOR LEFT_PAREN declaration expr? SEMI expr? RIGHT_PAREN arm
  (define (write-for expr)
    (match expr
      ;; for of and for in
      [('for (((and (or 'let 'const) decl) binding-expr)
              (and (or #:of #:in) of-or-in)
              for-rh-expr)
             body ...)
       (dop "for (")
       (dop (symbol->string decl))
       (dop " ")
       (write-id binding-expr)
       (dop " ")
       (dop (symbol->string (keyword->symbol of-or-in)))
       (dop " ")
       (write-expr binding-expr)
       (dop ") {")
       (up-indentation
        (write-block body))
       (newline-indent)
       (dop "}")]
      ;; TODO: C-style (two or?) three part or
      ;;
      ;; Actually I'm not sure if Jessie supports this, it looks
      ;; like maybe only the two part one.  I'm confused...
      ;; At any rate not all statements might be permitted in the
      ;; third part.  So I'm unsure.
      #;[('for (((and (or 'let 'const) decl) binding-expr)
              continue-loop-statement
              update-statement)
             body ...)
       (dop "for (")
       (dop (symbol->string decl))
       (dop " ")
       (write-id binding-expr)
       (dop "; ")
       (write-expr continue-loop-statement)
       (dop ") {")
       (up-indentation
        (write-block body))
       (dop "}")]
      )
      )
    

  (define (write-id expr)
    (unless (valid-id? expr)
      (error "Not a valid id" expr))
    (dop (symbol->string expr)))

  ;;   WHILE LEFT_PAREN expr RIGHT_PAREN arm
  (define (write-while expr)
    (match expr
      [('while condition body ...)
       (dop "while (")
       (write-expr condition)
       (write-expr ") {")
       (up-indentation
        (write-block body))
       (newline-indent)
       (write-expr "}")]))

  ;;   SWITCH LEFT_PAREN expr RIGHT_PAREN LEFT_BRACE clause* RIGHT_BRACE
  (define (write-switch expr)
    (define (_write-it sw-expr cases default)
      (dop "switch (")
      (write-expr sw-expr)
      (dop ") {")
      (up-indentation
       (for-each-nli
        (match-lambda
          [(case-matches-expr body ...)
           (dop "case ")
           (write-expr case-matches-expr)
           (dop ": ")
           (write-switch-case-body body)])
        cases)
       (when default
         (dop "default: ")
         (write-switch-case-body default)))
      (dop "}"))
    (define (write-switch-case-body expr)
      (match expr
        [(single-body-expr)
         (up-indentation
          (newline-indent)
          (write-expr single-body-expr))
         (newline-indent)]
        [(body-exprs ...)
         (dop "{")
         (up-indentation
          (write-block body-exprs))
         (newline-indent)
         (dop "}")]))
    (match expr
      [('switch sw-expr cases ... #:default (default-case ...))
       (_write-it sw-expr cases default-case)]
      [('switch sw-expr cases ...)
       (_write-it sw-expr cases #f)]))

  (define (write-import expr)
    (match expr
      [('import '* #:as (? valid-id? def-import) #:from (? string? import-path))
       (dop "import * as ")
       (dop (symbol->string def-import))
       (dop " from ")
       (write-string import-path)
       (dop ";")]
      [('import (? string? import-path))
       (dop "import ")
       (write-string import-path)
       (dop ";")]
      [('import (? valid-id? import-name) #:from (? string? import-path))
       (dop "import ")
       (dop (symbol->string import-name))
       (dop " from ")
       (write-string import-path)
       (dop ";")]
      [('import ((? valid-id? import-names) ...) #:from (? string? import-path))
       (dop "import {")
       (write-func-params import-names)
       (dop "} from ")
       (write-string import-path)
       (dop ";")]
      [('import (((? valid-id? import-names) #:as (? valid-id? bind-names)) ...)
                #:from (? string? import-path))
       (dop "import {")
       (write-func-params import-names)
       (dop " as ")
       (write-func-params bind-names)
       (dop "} from ")
       (write-string import-path)
       (dop ";")]))

  (define (write-export expr)
    (match expr
      ;; TODO: more to come
      [('export (? valid-id? export-ids) ...)
       (dop "export {")
       (write-func-params export-ids)
       (dop "};")]))

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
       (up-indentation
        (write-block body))
       (newline-indent)
       (dop "}")]))

  (define (write-function expr)
    (match expr
      ;; Arrow expressions in JS are... goofy.  They have implicit return
      ;; whereas normal functions don't, and they have a compact representation
      ;; that people seem to like, but given that the semantics are *not*
      ;; the same, we are cautious about when to write them out and default to
      ;; only when there's a an explicit return as the only part of a
      ;; procedure body.  Otherwise out we write out a more verbose
      ;; function representation.
      [(_ (params ...) ('return return-expr))
       (match params
         [((? valid-id? one-param))
          (write-id one-param)]
         [_
          (dop "(")
          (write-func-params params)
          (dop ")")])
       (dop " => ")
       (match return-expr
         ;; TODO: Are *all* expressions safe to not wrap in parens?
         ;;   dunno...
         ;; certain expressions 
         [(or (? valid-id?)
              #t #f 'true 'false 'null 'undefined
              (? number?))
          (write-expr return-expr)]
         ;; otherwise let's be careful and wrap in parens
         [_
          (dop "(")
          (write-expr return-expr)
          (dop ")")])]
      ;; and more vanilla functions...
      [(_ (params ...) body ...)
       (dop "function (")
       (write-func-params params)
       (dop ") {")
       (up-indentation
        (write-block body))
       (newline-indent)
       (dop "}")]))

  (define (write-func-params params)
    (define (write-param param)
      (match param
        [(? valid-id? id) (write-id id)]
        [((? valid-id? id) default-expr)
         (dop (symbol->string id))
         (dop " = ")
         (write-expr default-expr)]
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
         (write-expr val)]))
    (dop "{")
    (up-indentation
     (for-each-nli write-propdef expr #:sep ","))
    (newline-indent)
    (dop "}"))

  (define (write-catch-pattern expr)
    (error 'TODO))

  ;; @@: We never write exponents... that's ok, right?
  (define (write-number expr)
    (match expr
      [(or (? integer?) (? inexact?)) (dop (number->string expr))]))

  (write-top james-expr))

(display "** Simple Objects: stateless singleton **\n")
(display
 (james->jessie-str
  '(module
    (defconst origin (%r (: getX (fn () (return 0))) (: getY (fn () (return 0)))))
    (.-> console (.log (.-> origin (.getY)))))))

(display ".--------.\n")
(display "| maker: |\n")
(display "'--------'\n")
(display
 (james->jessie-str
  '(module
    (defconst makeCounter (fn (init)
                              (let value init)
                              (return (%r
                                       (: increment (fn () (+= value 1)))
                                       (: decrement (fn () (-= value 1)))
                                       (: makeOffsetCounter
                                          (fn (delta) (return
                                                       (makeCounter (+ value delta)))) )
                                       )) ))
    (defconst c1 (makeCounter 1))
    (.-> c1 (.increment))
    (.-> console (.log (.-> c1 (.decrement))))
    )))

(newline)
(display ".-------.\n")
(display "| mint: |\n")
(display "'-------'\n")
(display
 (james->jessie-str
  '(module
    (defn (makeMint)
      (defconst ledger (makeWeakMap))

      (defconst issuer
        (harden (%r (: makeEmptyPurse (fn () (.-> mint (.makePurse 0)))))))

      (defconst mint
        (harden
         (%r
          (: makePurse
             (fn (initialBalance)
                 (defconst purse
                   (harden
                    (%r
                     (: getIssuer (fn () (return issuer)))
                     (: getBalance (fn () (.-> ledger (.get purse))))

                     (: deposit (fn (amount src)
                                    (Nat (+ (.-> ledger (.get purse)) (Nat amount)))
                                    (.-> ledger (.set src (- (Nat (.-> ledger (.get src))) amount)))
                                    (.-> ledger (.set purse (+ (.-> ledger (.get purse)) amount)))
                                    ))
                     (: withdraw (fn (amount)
                                     (defconst newPurse (.-> issuer (.makeEmptyPurse)))
                                     (.-> newPurse (.deposit amount purse))
                                     (return newPurse)))
                     )))

                 (.-> ledger (.set purse initialBalance))
                 (return purse)
                 ))
          )))

      (return mint)
      )

    ;; example from http://erights.org/elib/capability/ode/ode-capabilities.html
    (defconst carolMint (makeMint))
    (defconst aliceMainPurse (.-> carolMint (.makePurse 1000)))
    (defconst bobMainPurse (.-> carolMint (.makePurse 0)))
    (defconst paymentForBob (.-> aliceMainPurse (.withdraw 10)))

    ;; (.-> bob (.foo paymentForBob))
    (.-> bobMainPurse (.deposit 10 paymentForBob))

    (.-> console (.log (.-> aliceMainPurse (.getBalance))))
    (.-> console (.log (.-> bobMainPurse (.getBalance))))
    )
  ))

(newline)
(display ".------------.\n")
(display "| hasher.js: |\n")
(display "'------------'\n")
(display
 (james->jessie-str '(module
                      (import (assert) #:from "@agoric/assert")
                      (import (createHash) #:from "crypto")
                      (defn (createSha256 [initial undefined])
                        (defconst hash
                          (createHash "sha256"))
                        (let done #f)
                        (defn (add more)
                          (assert (not done))
                          (hash.update more))
                        (defn (finish)
                          (assert (not done))
                          (set! done #t)
                          (return (hash.digest "hex")))
                        (if initial
                            (add initial))
                        (return (harden (%r (: add finish)))))
                      (harden createSHA256)
                      (export create))))


(newline)
(display ".---------------------------------.\n")
(display "| Nested math anti-infix example: |\n")
(display "'---------------------------------'\n")
(display
 (james->jessie-str '(+ 1 2 (* 3 4)
                        (numberFrob (- 42 2)
                                    88)
                        -55)))
