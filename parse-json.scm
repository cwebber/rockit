(define-module (language json parser)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 match)
  )


;;; JSON level
;;; ==========

(define-peg-pattern _WS none
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
(define-peg-pattern exp all
  (and (or "E" "e")
       (? (or "+" "-"))
       (+ digit)))
(define-peg-pattern int all
  (or (and (range #\1 #\9) (+ digit))
      (and MINUS (range #\1 #\9) (+ digit))
      (and MINUS digit)
      digit))


;; NUMBER <- < int frac? exp? > _WSN;
(define-peg-pattern NUMBER all
  (and (and int (? frac) (? exp)) _WSN))

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

(define-peg-pattern STRING all
  (and (ignore "\"") (* (and (not-followed-by "\"") character)) (ignore "\"") _WS))

;; dataLiteral <- (("null" / "false" / "true") _WSN / NUMBER / STRING) _WS;
(define-peg-pattern dataLiteral body
  (and (or (and (or "null" "false" "true") _WSN) NUMBER STRING) _WS))

;; pureArray <-
;;   LEFT_BRACKET pureExpr ** _COMMA _COMMA? RIGHT_BRACKET ${(_, es, _2) => ['array', es]};

(define-peg-pattern pureArray all
  (and LEFT_BRACKET pureExpr (* (and _COMMA (? _COMMA) pureExpr)) RIGHT_BRACKET))

;; array <-
;;   LEFT_BRACKET element ** _COMMA _COMMA? RIGHT_BRACKET ${(_, es, _2) => ['array', es]};

(define-peg-pattern array all
  (and LEFT_BRACKET element (* (and _COMMA (? _COMMA) element)) RIGHT_BRACKET))

;; # to be extended
;; element <- assignExpr;
(define-peg-pattern element body
  assignExpr)

;; # The JavaScript and JSON grammars calls records "objects"
;; 
;; pureRecord <-
;;   LEFT_BRACE purePropDef ** _COMMA _COMMA? RIGHT_BRACE  ${(_, ps, _2) => ['record', ps]};

(define-peg-pattern pureRecord all
  (and LEFT_BRACE purePropDef (* (and _COMMA (? _COMMA) purePropDef)) RIGHT_BRACE))

;; record <-
;;   LEFT_BRACE propDef ** _COMMA _COMMA? RIGHT_BRACE  ${(_, ps, _2) => ['record', ps]};

(define-peg-pattern record all
  (and LEFT_BRACE propDef (* (and _COMMA (? _COMMA) propDef)) RIGHT_BRACE))

;; dataStructure <-
;;   dataLiteral                             ${n => ['data', JSON.parse(n)]}
;; / array
;; / record
;; / HOLE                                    ${h => ['exprHole', h]};
(define-peg-pattern dataStructure body
  (or dataLiteral array record
      ;; TODO: HOLE
      ))

;; # to be extended
;; primaryExpr <- dataStructure;
(define-peg-pattern primaryExpr body
  dataStructure)

;; # An expression without side-effects.
;; # to be extended
;; pureExpr <-
;;   dataLiteral                             ${n => ['data', JSON.parse(n)]}
;; / pureArray
;; / pureRecord
;; / HOLE                                    ${h => ['exprHole', h]};

(define-peg-pattern pureExpr all
  (or dataLiteral pureArray pureRecord
      ;; TODO: HOLE
      ))

;; # to be overridden
;; assignExpr <- primaryExpr;
(define-peg-pattern assignExpr body
  primaryExpr)

;; 
;; 
;; 
;; 
;; 
;; # to be extended
;; purePropDef <- propName COLON pureExpr     ${(k, _, e) => ['prop', k, e]};

(define-peg-pattern purePropDef all
  (and propName COLON pureExpr))

;; # to be extended
;; propDef <- propName COLON assignExpr       ${(k, _, e) => ['prop', k, e]};

(define-peg-pattern propDef all
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
(define-peg-pattern propName all
  STRING)

;; # to be overridden or inherited
;; start <- _WS assignExpr _EOF                ${v => (..._a: any[]) => v};
(define-peg-pattern start body
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

(define (string->json str)
  (define ptree
    (peg:tree (match-pattern start str)))
  (define (flatten lst)
    (match lst
      [((inner-lst ...)) inner-lst]
      [other other]))
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
    (match (flatten elt)
      [('STRING string-elts ...)
       (join-string-elts string-elts)]
      [('record propdefs ...)
       (let lp ([propdefs propdefs]
                [alist '()])
         (match (flatten propdefs)
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
             (exp-part (and=> (assoc 'exp rest-int-data) cadr)))
         (and exp-part (error "oops, exp stuff not implemented yet"))
         (if frac-part
             (string->number (format #f "~a.~a" int-part frac-part))
             (string->number int-part)))]
      [('array items ...)
       (list->vector (map parse-dstructure items))]))
  (parse-dstructure ptree))
