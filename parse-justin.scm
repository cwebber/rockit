;;; (C) 2021 Christine Lemmer-Webber, released as Apache v2
;;; Also code borrowed from Agoric's "jessica" package, also Apache v2


(define-module (language justin parser)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 match))


;;; JSON level
;;; ==========

(define-peg-pattern JSON._WS none
  (* (or "\t" "\n" "\r" " ")))
;; _WSN is whitespace or a non-ident character...???
(define-peg-pattern _WSN none
  (and (not-followed-by
        (or "$" (range #\A #\Z) (range #\a #\z) "_"))
       _WS))

;; Lexical syntax

;; _EOF <- ~.;
(define-peg-pattern _EOF body (not-followed-by peg-any))
;; LEFT_BRACKET <- "[" _WS;
(define-peg-pattern LEFT_BRACKET body (and (ignore "[") _WS))
;; RIGHT_BRACKET <- "]" _WS;
(define-peg-pattern RIGHT_BRACKET body (and (ignore "]") _WS))
;; LEFT_BRACE <- "{" _WS;
(define-peg-pattern LEFT_BRACE body (and (ignore "{") _WS))
;; RIGHT_BRACE <- "}" _WS;
(define-peg-pattern RIGHT_BRACE body (and (ignore "}") _WS))
;; _COMMA <- "," _WS                     ${_ => SKIP};
(define-peg-pattern _COMMA none (and (ignore ",") _WS))
;; COLON <- ":" _WS;
(define-peg-pattern COLON body (and (ignore ":") _WS))
;; MINUS <- "-" _WS;
(define-peg-pattern MINUS body (and "-" _WS))
;; HOLE <- &${HOLE} _WS;
;;; TODO: uhhh????  I'm guessing this is where Justin/Jessie slots in


(define-peg-pattern digit body
  (range #\0 #\9))
(define-peg-pattern frac all
  (and "." (+ digit)))
(define-peg-pattern exponent all
  (and (or "E" "e")
       (? (or "+" "-"))
       (+ digit)))
(define-peg-pattern int all
  (or (and (range #\1 #\9) (+ digit))
      (and MINUS (range #\1 #\9) (+ digit))
      (and MINUS digit)
      digit))


;; NUMBER <- < int frac? exponent? > _WSN;
(define-peg-pattern NUMBER all
  (and (and int (? frac) (? exponent)) _WSN))

;; utf8cont <- [\x80-\xbf];

(define-peg-pattern utf8cont body
  (range #\x80 #\xbf))

;; utf8 <-
;;   [\xc2-\xdf] utf8cont
;; / [\xe0-\xef] utf8cont utf8cont
;; / [\xf0-\xf4] utf8cont utf8cont utf8cont;

(define-peg-pattern utf8 body
  (or (range #\xc2 #\xdf) utf8cont))

;; escape <- '\\' ['"\\bfnrt];
(define-peg-pattern escape all
  (and (ignore "\\") (or "'" "\"" "\\" "b" "f" "n" "r" "t")))


;; hex <- digit / [a-fA-F];
(define-peg-pattern hex body
  (or digit (range #\a #\f) (range #\A #\F)))

;; character <-
;;   escape
;; / '\\u' hex hex hex hex
;; / ~'\\' ([\x20-\x7f] / utf8);

(define-peg-pattern character body
  (or escape
      hex-char
      (and (not-followed-by "\\")
           (or (range #\x20 #\x7f)
               utf8))))

(define-peg-pattern hex-char all
  (and (ignore "\\u") hex hex hex hex))

;; STRING <- < '"' (~'"' character)* '"' > _WS;

(define-peg-pattern JSON.STRING all
  (and (ignore "\"") (* (and (not-followed-by "\"") character)) (ignore "\"") _WS))

;; dataLiteral <- (("null" / "false" / "true") _WSN / NUMBER / STRING) _WS;
(define-peg-pattern dataLiteral body
  (and (or (and (or "null" "false" "true") _WSN) NUMBER STRING) _WS))

;; pureArray <-
;;   LEFT_BRACKET pureExpr ** _COMMA _COMMA? RIGHT_BRACKET ${(_, es, _2) => ['array', es]};

;; (define-peg-pattern pureArray all
;;   (and LEFT_BRACKET pureExpr (* (and _COMMA pureExpr)) (? _COMMA) RIGHT_BRACKET))

;; array <-
;;   LEFT_BRACKET element ** _COMMA _COMMA? RIGHT_BRACKET ${(_, es, _2) => ['array', es]};

(define-peg-pattern array all
  (and LEFT_BRACKET (? (and element (* (and _COMMA element)) (? _COMMA))) RIGHT_BRACKET))

;; # to be extended
;; element <- assignExpr;
(define-peg-pattern JSON.element body
  assignExpr)

;; # The JavaScript and JSON grammars calls records "objects"
;; 
;; pureRecord <-
;;   LEFT_BRACE purePropDef ** _COMMA _COMMA? RIGHT_BRACE  ${(_, ps, _2) => ['record', ps]};

;; (define-peg-pattern pureRecord all
;;   (and LEFT_BRACE purePropDef (* (and _COMMA (? _COMMA) purePropDef)) RIGHT_BRACE))

;; record <-
;;   LEFT_BRACE propDef ** _COMMA _COMMA? RIGHT_BRACE  ${(_, ps, _2) => ['record', ps]};
(define-peg-pattern JSON.record all
  (and LEFT_BRACE (? (and propDef (* (and _COMMA propDef)) (? _COMMA))) RIGHT_BRACE))

;; dataStructure <-
;;   dataLiteral                             ${n => ['data', JSON.parse(n)]}
;; / array
;; / record
;; / HOLE                                    ${h => ['exprHole', h]};
(define-peg-pattern JSON.dataStructure body
  (or dataLiteral array record
      ;; TODO: HOLE
      ))

;; # to be extended
;; primaryExpr <- dataStructure;
(define-peg-pattern JSON.primaryExpr body
  dataStructure)

;; # An expression without side-effects.
;; # to be extended
;; pureExpr <-
;;   dataLiteral                             ${n => ['data', JSON.parse(n)]}
;; / pureArray
;; / pureRecord
;; / HOLE                                    ${h => ['exprHole', h]};

;; (define-peg-pattern pureExpr all
;;   (or dataLiteral pureArray pureRecord
;;       ;; TODO: HOLE
;;       ))

;; # to be overridden
;; assignExpr <- primaryExpr;
;; (define-peg-pattern assignExpr body
;;   primaryExpr)

;; 
;; 
;; 
;; 
;; 
;; # to be extended
;; purePropDef <- propName COLON pureExpr     ${(k, _, e) => ['prop', k, e]};

;; (define-peg-pattern purePropDef all
;;   (and propName COLON pureExpr))

;; # to be extended
;; propDef <- propName COLON assignExpr       ${(k, _, e) => ['prop', k, e]};

(define-peg-pattern JSON.propDef all
  (and propName COLON assignExpr))

;; # to be extended
;; propName <- STRING                     ${(str) => {
;;                                             const js = JSON.parse(str);
;;                                             if (js === '__proto__') {
;;                                               // Don't allow __proto__ behaviour attacks.
;;                                               return FAIL;
;;                                             }
;;                                             return ['data', js];
;;                                           }};
;; 
(define-peg-pattern JSON.propName all
  #;(and (not-followed-by (and "__proto__" _WSN)))
  STRING)

;; # to be overridden or inherited
;; start <- _WS assignExpr _EOF                ${v => (..._a: any[]) => v};
(define-peg-pattern JSON.start body
   (and _WS assignExpr _EOF))

(define (string->json-ptree str)
  (peg:tree (match-pattern start str)))

(define (hex-str->int str)
  (define str-len (string-length str))
  (let lp ([place 0]
           [int 0])
    (if (= str-len place)
        int
        (let* ([this-digit
                (match (string-ref str (1- (- str-len place)))
                  [#\0 0]
                  [#\1 1]
                  [#\2 2]
                  [#\3 3]
                  [#\4 4]
                  [#\5 5]
                  [#\6 6]
                  [#\7 7]
                  [#\8 8]
                  [#\9 9]
                  [(or #\a #\A) 10]
                  [(or #\b #\B) 11]
                  [(or #\c #\C) 12]
                  [(or #\d #\D) 13]
                  [(or #\e #\E) 14]
                  [(or #\f #\F) 15])]
               [next-int
                (+ int
                   (* this-digit (expt 16 place)))])
          (lp (1+ place) next-int)))))

#;(define (string->json str)
  (define ptree
    (peg:tree (match-pattern start str)))
  (define (join-string-elts string-elts)
    (apply string-append
           (map (match-lambda
                  [(? string? str) str]
                  [('escape "'") "'"]
                  [('escape "\"") "\""]
                  [('escape "\\") "\\"]
                  [('escape "b") "\b"]
                  [('escape "f") "\f"]
                  [('escape "n") "\n"]
                  [('escape "r") "\r"]
                  [('escape "t") "\t"]
                  [('hex-char hex-str)
                   (list->string (list (integer->char (hex-str->int hex-str))))])
                string-elts)))
  (define (parse-dstructure elt)
    (match elt
      [('STRING string-elts ...)
       (join-string-elts string-elts)]
      [('record propdefs ...)
       (let lp ([propdefs (keyword-flatten '(propDef) propdefs)]
                [alist '()])
         (match propdefs
           ['() alist]
           [('propDef ('propName ('STRING key-string-elts ...))
                      val)
            (cons (cons (join-string-elts key-string-elts)
                        (parse-dstructure val))
                  alist)]
           [(('propDef ('propName ('STRING key-string-elts ...))
                       val)
             rest-items ...)
            (lp rest-items
                (cons (cons (join-string-elts key-string-elts)
                            (parse-dstructure val))
                      alist))]))]
      [('NUMBER ('int int-part) rest-int-data ...)
       (let ((frac-part (and=> (assoc 'int rest-int-data) cadr))
             (exponent-part (and=> (assoc 'exponent rest-int-data) cadr)))
         (and exponent-part (error "oops, exponent stuff not implemented yet"))
         (if frac-part
             (string->number (format #f "~a.~a" int-part frac-part))
             (string->number int-part)))]
      [('array items ...)
       (list->vector (map parse-dstructure items))]))
  (parse-dstructure ptree))

;;; Justin level
;;; ============

;; # to be overridden or inherited
;; start <- _WS assignExpr _EOF                       ${v => (..._a: any[]) => v};
(define-peg-pattern start body
  (and _WS assignExpr _EOF))

;; # A.1 Lexical Grammar
;; 
;; DOT <- "." _WS;
(define-peg-pattern DOT all
  (and (ignore ".") _WS))

;; ELLIPSIS <- "..." _WS;
(define-peg-pattern ELLIPSIS all
  (and (ignore "...") _WS))

;; LEFT_PAREN <- "(" _WS;
(define-peg-pattern LEFT_PAREN body
  (and "(" _WS))

;; PLUS <- "+" _WS;
(define-peg-pattern PLUS all
  (and (ignore "+") _WS))

;; QUESTION <- "?" _WS;
(define-peg-pattern QUESTION all
  (and (ignore "?") _WS))

;; RIGHT_PAREN <- ")" _WS;
(define-peg-pattern RIGHT_PAREN body
  (and ")" _WS))

;; STARSTAR <- "**" _WS;
(define-peg-pattern STARSTAR all
  (and (ignore "**") _WS))

;; # Define Javascript-style comments.
;; _WS <- super._WS (EOL_COMMENT / MULTILINE_COMMENT)?   ${_ => SKIP};
(define-peg-pattern _WS none
  (and (* (or "\t" "\n" "\r" " "))               ; JSON
       (? (or EOL_COMMENT MULTILINE_COMMENT))))  ; +Justin

;; EOL_COMMENT <- "//" (~[\n\r] .)* _WS;
(define-peg-pattern EOL_COMMENT none
  (and "//" (* (and (not-followed-by (or "\n" "\r")) peg-any)) _WS))

;; MULTILINE_COMMENT <- "/*" (~"*/" .)* "*/" _WS;
(define-peg-pattern MULTILINE_COMMENT none
  (and "/*" (* (and (not-followed-by "*/") peg-any)) "*/" _WS))

;; # Add single-quoted strings.
;; STRING <-
;;   super.STRING
;; / "'" < (~"'" character)* > "'" _WS  ${s => transformSingleQuote(s)};
(define-peg-pattern STRING all
  (or
   ;; JSON
   (and (ignore "\"")
        (* (and (not-followed-by "\"") character))
        (ignore "\"") _WS)
   ;; +Justin
   (and (ignore "'")
        (* (and (not-followed-by "'") character))
        (ignore "'") _WS)))

;; # Only match if whitespace doesn't contain newline
;; _NO_NEWLINE <- ~IDENT [ \t]*     ${_ => SKIP};
(define-peg-pattern _NO_NEWLINE none
  (and (not-followed-by IDENT)
       (* (or " " "\t"))))

;; IDENT_NAME <- ~(HIDDEN_PFX / "__proto__") (IDENT / RESERVED_WORD);
(define-peg-pattern IDENT_NAME all
  (and
   (not-followed-by (or HIDDEN_PFX "__proto__"))
   (or IDENT RESERVED_WORD)))

;; IDENT <-
;;   ~(HIDDEN_PFX / IMPORT_PFX / RESERVED_WORD)
;;   < [$A-Za-z_] [$A-Za-z0-9_]* > _WSN;
(define-peg-pattern IDENT all
  (and
   ;; TODO: Shouldn't _WSN go in here?  At least for the _PFX ones?
   ;; TODO: And doesn't the RESERVED_WORD here block words prefixed by
   ;;   some kid of reserved word?
   ;; TODO: Really, maybe some of this belongs in rejecting certain
   ;;   terms in the post-read step
   (not-followed-by (or HIDDEN_PFX IMPORT_PFX RESERVED_WORD))
   (or (range #\A #\Z) (range #\a #\z) "_")
   (* (or (range #\A #\Z) (range #\a #\z) (range #\0 #\9) "_"))
   _WSN))

;; HIDDEN_PFX <- "$h_";
(define-peg-pattern HIDDEN_PFX all
  "$h_")

;; IMPORT_PFX <- "$i_";
(define-peg-pattern IMPORT_PFX all
  "$i_")

;; # Omit "async", "arguments", "eval", "get", and "set" from IDENT
;; # in Justin even though ES2017 considers them in IDENT.
;; RESERVED_WORD <-
;;   (KEYWORD / RESERVED_KEYWORD / FUTURE_RESERVED_WORD
;; / "null" / "false" / "true"
;; / "async" / "arguments" / "eval" / "get" / "set") _WSN;
(define-peg-pattern RESERVED_WORD all
  (and (or KEYWORD RESERVED_KEYWORD FUTURE_RESERVED_WORD
           "null" "false" "true"
           "async" "arguments" "eval" "get" "set")
       _WSN))

;; KEYWORD <-
;;   ("break"
;; / "case" / "catch" / "const" / "continue"
;; / "debugger" / "default"
;; / "else" / "export"
;; / "finally" / "for" / "function"
;; / "if" / "import"
;; / "return"
;; / "switch"
;; / "throw" / "try" / "typeof"
;; / "void"
;; / "while") _WSN;
(define-peg-pattern KEYWORD all
  (and (or "break"
           "case" "catch" "const" "continue"
           "debugger" "default"
           "else" "export"
           "finally" "for" "function"
           "if" "import"
           "return"
           "switch"
           "throw" "try" "typeof"
           "void"
           "while")
       _WSN))

;; # Unused by Justin but enumerated here, in order to omit them
;; # from the IDENT token.
;; RESERVED_KEYWORD <-
;;   ("class"
;; / "delete" / "do"
;; / "extends"
;; / "instanceof"
;; / "in"
;; / "new"
;; / "super"
;; / "this"
;; / "var"
;; / "with"
;; / "yield") _WSN;
(define-peg-pattern RESERVED_KEYWORD all
  (and (or "class"
           "delete" "do"
           "extends"
           "instanceof"
           "in"
           "new"
           "super"
           "this"
           "var"
           "with"
           "yield")
       _WSN))

;; FUTURE_RESERVED_WORD <-
;;   ("await" / "enum"
;; / "implements" / "package" / "protected"
;; / "interface" / "private" / "public") _WSN;
(define-peg-pattern FUTURE_RESERVED_WORD all
  (and (or "await" "enum"
           "implements" "package" "protected"
           "interface" "private" "public")
       _WSN))

;; # Quasiliterals aka template literals
;; QUASI_CHAR <- "\\" . / ~"\`" .;
(define-peg-pattern QUASI_CHAR all
  (or (and "\\" peg-any)
      (and (not-followed-by "`") peg-any)))
;; QUASI_ALL <- "\`" < (~"\${" QUASI_CHAR)* > "\`" _WS;
(define-peg-pattern QUASI_ALL all
  (and (ignore "`")
       (* (and (not-followed-by "${") QUASI_CHAR))
       (ignore "`")
       _WS))
;; QUASI_HEAD <- "\`" < (~"\${" QUASI_CHAR)* > "\${" _WS;
(define-peg-pattern QUASI_HEAD all
  (and (ignore "`")
       (* (and (not-followed-by "${") QUASI_CHAR))
       (ignore "${")
       _WS))

;; QUASI_MID <- "}" < (~"\${" QUASI_CHAR)* > "\${" _WS;
(define-peg-pattern QUASI_MID all
  (and (ignore "}")
       (* (and (not-followed-by "${") QUASI_CHAR))
       (ignore "${")
       _WS))

;; QUASI_TAIL <- "}" < (~"\${" QUASI_CHAR)* > "\`" _WS;
(define-peg-pattern QUASI_TAIL all
  (and (ignore "}")
       (* (and (not-followed-by "${") QUASI_CHAR))
       (ignore "${")
       _WS))

;; # A.2 Expressions
;; 
;; undefined <-
;;   "undefined" _WSN     ${_ => ['undefined']};

(define-peg-pattern undefined all
  (and "undefined" _WSN))

;; dataStructure <-
;;   undefined
;; / super.dataStructure;

(define-peg-pattern dataStructure all
  (or undefined dataLiteral array record))

;; .... looks the same as the json version
;; # Optional trailing commas.
;; record <-
;;   super.record
;; / LEFT_BRACE propDef ** _COMMA _COMMA? RIGHT_BRACE      ${(_, ps, _2) => ['record', ps]};
(define-peg-pattern record all
  (and LEFT_BRACE (? (and propDef
                          (* (and _COMMA propDef))
                          (? _COMMA)))
       RIGHT_BRACE))

;; NOTE: This seems the same as with the json so I'm not changing it
;; array <-
;;   super.array
;; / LEFT_BRACKET element ** _COMMA _COMMA? RIGHT_BRACKET  ${(_, es, _2) => ['array', es]};

;; useVar <- IDENT                                       ${id => ['use', id]};

(define-peg-pattern useVar all
  IDENT)

;; # Justin does not contain variable definitions, only uses. However,
;; # multiple languages that extend Justin will contain defining
;; # occurrences of variable names, so we put the defVar production
;; # here.
;; defVar <- IDENT                                       ${id => ['def', id]};

(define-peg-pattern defVar all
  IDENT)

;; primaryExpr <-
;;   super.primaryExpr
;; / quasiExpr
;; / LEFT_PAREN expr RIGHT_PAREN                         ${(_, e, _2) => e}
;; / useVar;

(define-peg-pattern primaryExpr all
  (or dataStructure  ; JSON
      ;; everything else
      quasiExpr
      ;; Wrap these _PARENs in ignore?
      (and LEFT_PAREN expr RIGHT_PAREN)
      useVar))

;; pureExpr <-
;;   super.pureExpr
;; / LEFT_PAREN pureExpr RIGHT_PAREN                     ${(_, e, _2) => e}
;; / useVar;
;; IGNORING THESE PURE THINGS FOR NOW????


;; element <-
;;   super.element
;; / ELLIPSIS assignExpr                                 ${(_, e) => ['spread', e]};

(define-peg-pattern element body
  (or assignExpr (and ELLIPSIS assignExpr)))

;; propDef <-
;;   super.propDef
;; / useVar                                              ${u => ['prop', u[1], u]}
;; / ELLIPSIS assignExpr                                 ${(_, e) => ['spreadObj', e]};
(define-peg-pattern propDef all
  (or (and propName COLON assignExpr)
      useVar
      (and ELLIPSIS assignExpr)))

;; purePropDef <-
;;   super.purePropDef
;; / useVar                                              ${u => ['prop', u[1], u]}
;; / ELLIPSIS assignExpr                                 ${(_, e) => ['spreadObj', e]};
;; 
;; # No computed property name
;; propName <-
;;   super.propName
;; / IDENT_NAME
;; / NUMBER;

(define-peg-pattern propName all
  (or (and (not-followed-by (and "__proto__" _WS))
           STRING)
      IDENT_NAME
      NUMBER))

;; quasiExpr <-
;;   QUASI_ALL                                            ${q => ['quasi', [q]]}
;; / QUASI_HEAD expr ** QUASI_MID QUASI_TAIL              ${(h, ms, t) => ['quasi', qunpack(h, ms, t)]};

(define-peg-pattern quasiExpr all
  (or QUASI_ALL
      (and QUASI_HEAD
           (? (and expr (* (and QUASI_MID expr))))
           QUASI_TAIL)))

;; # to be extended
;; memberPostOp <-
;;   LEFT_BRACKET indexExpr RIGHT_BRACKET                 ${(_, e, _3) => ['index', e]}
;; / DOT IDENT_NAME                                       ${(_, id) => ['get', id]}
;; / quasiExpr                                            ${q => ['tag', q]};

(define-peg-pattern memberPostOp all
  (or (and LEFT_BRACKET indexExpr RIGHT_BRACKET)
      (and DOT IDENT_NAME)
      quasiExpr))

;; # to be extended
;; callPostOp <-
;;   memberPostOp
;; / args                                                 ${args => ['call', args]};
(define-peg-pattern callPostOp all
  (or memberPostOp
      args))

;; # Because Justin and Jessie have no "new" or "super", they don't need
;; # to distinguish callExpr from memberExpr. So justin omits memberExpr
;; # and newExpr. Instead, in Justin, callExpr jumps directly to
;; # primaryExpr and updateExpr jumps directly to callExpr.
;; 
;; # to be overridden.
;; callExpr <- primaryExpr callPostOp*                   ${binary};

(define-peg-pattern callExpr all
  (and primaryExpr (* callPostOp)))

;; # To be overridden rather than inherited.
;; # Introduced to impose a non-JS restriction
;; # Restrict index access to number-names, including
;; # floating point, NaN, Infinity, and -Infinity.
;; indexExpr <-
;;   NUMBER                                               ${n => ['data', JSON.parse(n)]}
;; / PLUS unaryExpr                                       ${(_, e) => [`pre:+`, e]};

(define-peg-pattern indexExpr all
  (or NUMBER (and PLUS unaryExpr)))

;; args <- LEFT_PAREN arg ** _COMMA RIGHT_PAREN            ${(_, args, _2) => args};
(define-peg-pattern args all
  (and (ignore LEFT_PAREN)
       (? (and arg (* (and _COMMA arg))))
       (ignore RIGHT_PAREN)))

;; arg <-
;;   assignExpr
;; / ELLIPSIS assignExpr                                  ${(_, e) => ['spread', e]};
(define-peg-pattern arg body
  (or assignExpr
      (and ELLIPSIS assignExpr)))

;; # to be overridden
;; updateExpr <- callExpr;
(define-peg-pattern updateExpr all
  callExpr)

;; unaryExpr <-
;;   preOp unaryExpr                                      ${(op, e) => [op, e]}
;; / updateExpr;
(define-peg-pattern unaryExpr all
  (or (and preOp unaryExpr)
      updateExpr))

;; # to be extended
;; # No prefix or postfix "++" or "--".
;; # No "delete".
;; preOp <- (("void" / "typeof") _WSN / prePre);
(define-peg-pattern preOp all
  (or (and (or "void" "typeOf") _WSN)
      prePre))
;; prePre <- ("+" / "-" / "~" / "!") _WS                 ${op => `pre:${op}`};
(define-peg-pattern prePre body
  (and (or "+" "-" "~" "!") _WS))

;; # Different communities will think -x**y parses in different ways,
;; # so the EcmaScript grammar forces parens to disambiguate.
;; powExpr <-
;;   updateExpr STARSTAR powExpr                          ${(x, op, y) => [op, x, y]}
;; / unaryExpr;
(define-peg-pattern powExpr all
  (or (and updateExpr STARSTAR powExpr)
      unaryExpr))

;; multExpr <- powExpr (multOp powExpr)*                  ${binary};
(define-peg-pattern multExpr all
  (and powExpr (* (and multOp powExpr))))
;; addExpr <- multExpr (addOp multExpr)*                  ${binary};
(define-peg-pattern addExpr all
  (and multExpr (* (and addOp multExpr))))
;; shiftExpr <- addExpr (shiftOp addExpr)*                ${binary};
(define-peg-pattern shiftExpr all
  (and addExpr (* (and shiftOp addExpr))))

;; # Non-standard, to be overridden
;; # In C-like languages, the precedence and associativity of the
;; # relational, equality, and bitwise operators is surprising, and
;; # therefore hazardous. Here, none of these associate with the
;; # others, forcing parens to disambiguate.

;; eagerExpr <- shiftExpr (eagerOp shiftExpr)?            ${binary};
(define-peg-pattern eagerExpr all
  (and shiftExpr (? (and eagerOp shiftExpr))))

;; andThenExpr <- eagerExpr (andThenOp eagerExpr)*       ${binary};
(define-peg-pattern andThenExpr all
  (and eagerExpr (* (and andThenOp eagerExpr))))

;; orElseExpr <- andThenExpr (orElseOp andThenExpr)*     ${binary};
(define-peg-pattern orElseExpr all
  (and andThenExpr (* (and orElseOp andThenExpr))))

;; multOp <- ("*" / "/" / "%") _WS;
(define-peg-pattern multOp all
  (and (or "*" "/" "%") _WS))
;; addOp <- ("+" / "-") _WS;
(define-peg-pattern addOp all
  (and (or "+" "-") _WS))
;; shiftOp <- ("<<" / ">>>" / ">>") _WS;
(define-peg-pattern shiftOp all
  (and (or "<<" ">>>" ">>") _WS))
;; relOp <- ("<=" / "<" / ">=" / ">") _WS;
(define-peg-pattern relOp all
  (and (or "<=" "<" ">=" ">") _WS))
;; eqOp <- ("===" / "!==") _WS;
(define-peg-pattern eqOp all
  (and (or "===" "!==") _WS))
;; bitOp <- ("&" / "^" / "|") _WS;
(define-peg-pattern bitOp all
  (and (or "&" "^" "|") _WS))

;; eagerOp <- relOp / eqOp / bitOp;
(define-peg-pattern eagerOp all
  (or relOp eqOp bitOp))

;; andThenOp <- "&&" _WS;
(define-peg-pattern andThenOp all
  (ignore (and "&&" _WS)))

;; orElseOp <- "||" _WS;
(define-peg-pattern orElseOp all
  (ignore (and "||" _WS)))

;; condExpr <-
;;   orElseExpr QUESTION assignExpr COLON assignExpr   ${(c, _, t, _2, e) => ['cond', c, t, e]}
;; / orElseExpr;
(define-peg-pattern condExpr all
  (or (and orElseExpr QUESTION assignExpr COLON assignExpr)
      orElseExpr))

;; # override, to be extended
;; assignExpr <- condExpr;
(define-peg-pattern assignExpr all
  condExpr)

;; # The comma expression is not in Jessie because we
;; # opt to pass only insulated expressions as the this-binding.
;; expr <- assignExpr;
(define-peg-pattern expr all
  assignExpr)
