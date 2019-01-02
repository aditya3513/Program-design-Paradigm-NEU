;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 program-to-strings
 make-def
 make-varexp
 make-appexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DFINITIONS:

;; A Program is a ListOfDefinition.
;; template:
;; program-fn : Program -> ??
;; (define (program-fn p)
;;   (cond
;;     [(empty? p) empty]
;;     [else ...
;;          (cons (definition-fn (first p))
;;                (program-fn (rest p)))]))


(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;; template:
;; def-fn : Definition -> ??
;; (define (def-fn def)
;;   (...(def-name def)
;;       (def-args def)
;;       (def-body def)))

(define-struct varexp (name))
(define-struct appexp (fn args))
;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en
;; Template:
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn varexp)
;;   (...(varexp-name varexp)))

;; appexp-fn : Appexp -> ??
;; (define (appexp-fn appexp)
;;   (...(appexp-fn varexp)
;;       (appexp-args varexp)))


;; A Variable is a Symbol.


(define sample_program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'y 'a-very-long-variable-name)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;          following the formatting rules described below.
;; EXAMPLE: see in tests
;; HALTING MEASURE: the length of program
;; DESIGN STRATEGY: use template of program
(define (program-to-strings program width)
  (cond
    [(empty? program) empty]
    [else
     (append (definition-to-strings (first program) width)
             (program-to-strings (rest program) width))]))

;; TEST :

(define l1 (list
 "def a-very-long-function-name (x) :"
 "    f1(x)"
 "def f2 (x,"
 "        y,"
 "        a-very-long-variable-name) :"
 "    f1(y)"
 "def f3 (x,"
 "        z,"
 "        t,"
 "        u) :"
 "    f1"
 "     (f2,"
 "      z,"
 "      f1"
 "       (f2,"
 "        z))"))

(begin-for-test
  (check-equal? (program-to-strings sample_program 2) l1
                "test for program-to-string fail"))


;; definition-to-strings : Definition PosInt -> ListOfString
;; GIVEN: A Definition def and width
;; RETURN: A list of strings contains the definition of def
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of Definition
(define (definition-to-strings def width)
  (append (make-definition-name def width)
          (to-list (make-exp (def-body def) 4 width))))

;; TEST :
(define DEF1 (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x)))))

(begin-for-test
  (check-equal? (definition-to-strings DEF1 1)
                (list "def a-very-long-function-name (x) :" "    f1(x)")
                "test for defination-to-string fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-definition-name : Definition PosInt -> ListOfString
;; GIVEN: A Definition def and a PosInt width
;; WHERE: every line of def's presentation cannot longer than width
;; RETURN: A ListOfString contains presentation of def
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: divide into cases on length of def
(define (make-definition-name def width)
  (if (or (< (get-length-of-definition def) width)
          (< (length (def-args def)) 2))
      (make-definition-short def)
      (make-definition-long def)))

;; TEST :
(begin-for-test
  (check-equal? (make-definition-name DEF1 1)
                (list "def a-very-long-function-name (x) :")
                "make-definition test fail"))


;; make-definition-short : Definition -> ListOfStrings
;; GIVEN:A Definition def
;; WHERE: the presentation def can fit into one line 
;; RETURN: the presentation of def
;; EXAMPLE: see in tests
;; DESIGN STRSTEGY: use template of Definition
(define (make-definition-short def)
  (to-list (string-append
            "def "
            (symbol->string (def-name def))
            " ("
            (make-args-list (def-args def))
            ") :")))

;; TEST :
(begin-for-test
  (check-equal? (make-definition-short DEF1)
                (list "def a-very-long-function-name (x) :")
                "make-definition-short test fail"))

;; make-args-list : ListOfExp -> String
;; GIVEN: A ListOfExp args
;; RETURN: A string contains all exp in args divided with comma
;; EXAMPLE: see in tests
;; HALTING MEASURE: the length of args
;; DESIGN STRATEGY: divided into case on length of args
(define (make-args-list args)
  (cond
    [(equal? (length args) 1)
     (symbol->string (first args))]
    [else
     (string-append (symbol->string (first args))
                    ","
                    (make-args-list (rest args)))]))

;; TESTS :
(begin-for-test
  (check-equal? (make-args-list (list 'x))
                "x" "make-args test fail")
  (check-equal? (make-args-list (list 'x 'y))
                "x,y" "make-args test fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-definition-long : Definition -> ListOfString
;; GIVEN: A Definition def
;; WHERE: The presentation of def can not fit into the given length 
;; RETURN: A list of string contains the presentation of def
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of def
(define (make-definition-long def)
  (cons (long-definition-first-line def)
        (long-definition-args-list (string-length
                                    (symbol->string (def-name def)))
                                   (rest (def-args def)))))

;; TEST:
(begin-for-test
  (check-equal?
   (make-definition-long
    (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y)))))
   (list "def f2 (x," "        a-very-long-variable-name," "        y) :")
   "The result is wrong."))


;; long-definition-first-line: Definition -> String
;; GIVEN: A Definition def
;; WHERE: the def should be presented in seperate lines
;; RETURN: the first line presentation of the definition
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of def
(define (long-definition-first-line def)
  (string-append "def "
                 (symbol->string (def-name def))
                 " ("
                 (symbol->string (first (def-args def)))
                 ","))

;; TEST:
(begin-for-test
  (check-equal?
   (long-definition-first-line
    (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y)))))
   "def f2 (x,"
   "The result is wrong."))


;; long-definition-args-list : PosInt ListOfExp -> ListOfString
;; GIVEN: A PosInt name-length and ListOfExp args
;; WHERE: name-length is the length of definition's name
;; RETURN: A list of string contains the body of definition
;; EXAMPLE: see in tests
;; HALTING MEASURE: length of args
;; DESIGN STRATEGY: use template of Definition
(define (long-definition-args-list name-length args)
  (cond
    [(equal? (length args) 1)
     (list (string-append (make-indent (+ 6 name-length))
                          (symbol->string (first args))
                          ") :"))]
    [else
     (append (list (string-append
                    (make-indent (+ 6 name-length))
                    (symbol->string (first args))
                    ","))
             (long-definition-args-list name-length (rest args)))]))

;; TEST:
(begin-for-test
  (check-equal?
   (long-definition-args-list 2 (list 'x 'a-very-long-variable-name 'y))
   (list "        x," "        a-very-long-variable-name," "        y) :")
   "The result is wrong."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-length-of-definition: Definition -> PosInt
;; GIVEN: A Definition def
;; RETURN: The length of the string of this definition
;;               contains space, parenthesis and colon
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of Definition
(define (get-length-of-definition def)
  (+ 9
     (get-symbol-length (def-name def))
     (get-length-of-definition-args (def-args def))
     (length (def-args def))))

;; TEST :
(begin-for-test
  (check-equal?
   (get-length-of-definition  (make-def 'a-very-long-function-name
                                        (list 'x)
                                        (make-appexp 'f1 (list (make-varexp 'x)))))
   36
   "The result is wrong."))


;; get-length-of-definition-args: ListOfExp -> NonNegInt
;; GIVEN: A ListOfExp args
;; RETURN: the length of the string contains all args divided with comma
;; EXAMPLE: see in tests
;; DESIGN STRATEGY:
(define (get-length-of-definition-args args)
  (local (;; get-length-of-definition-args : Exp Int -> PosInt
          ;; RETURN: the sum of arg's length and n
          (define (add-length arg n)
            (+ n (get-symbol-length arg))))
    (foldr add-length 0 args)))

;; TEST :
(begin-for-test
  (check-equal? (get-length-of-definition-args (list 'x)) 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-exp : Exp NonNegInt PosInt -> ListOfString
;; GIVEN: A Exp exp , a NonNegInt indent and a PosInt width
;; WHERE: indent means current indent for exp,
;;        width is the limitation of width for one line
;; RETURN:a representation of the expression as a sequence of lines
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of Exp
(define (make-exp exp indent width)
  (cond
    [(varexp? exp)
     (add-indent indent
                 (symbol->string (varexp-name exp)))]
    [(or (< (+ (get-length-of-exp exp) indent) width)
         (equal? 1 (length (appexp-args exp))))
     (add-indent indent (make-one-line-exp exp))]
    [(< (+ (get-length-to-first-arg exp) indent) width)
     (make-fn-with-first-arg exp indent width)]
    [else
     (make-fn-diff-args exp indent width)]))

;; TEST :
(begin-for-test
  (check-equal? (make-exp (make-appexp 'f1 (list (make-varexp 'x))) 1 5)
                " f1(x)" "make-exp test 1 fail")
  (check-equal? (make-exp (make-appexp
                           'f1 (list (make-appexp
                                      'f2 (list (make-varexp 'z)
                                                (make-varexp 'y)))
                                     (make-varexp 'z))) 1 10)
                (list " f1" "  (f2(z, y)," "   z)") "make-exp test 2 fail")
  (check-equal? (make-exp
                 (make-appexp
                  'f1
                  (list (make-appexp
                         'f2 (list (make-varexp 'z)
                                   (make-varexp 'y)))
                        (make-varexp 'z)
                        (make-appexp
                         'f1 (list (make-appexp
                                    'f2 (list (make-varexp 'z)
                                              (make-varexp 'y)))
                                   (make-varexp 'z))))) 4 20)
                (list "    f1(f2(z, y),"
                      "       z,"
                      "       f1(f2(z, y),"
                      "          z))")
                "The result is wrong."))



;; make-fn-with-first-arg : Exp NonNegInt PosInt -> List
;; GIVEN: A Exp exp , a NonNegInt indent and PosInt width
;; WHERE: the exp should be presented that much of the call is placed on a single line and
;;       each of the remaining argument expressions begins on a separate line
;; RETURN: the presentation of given exp
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of appexp
(define (make-fn-with-first-arg exp indent width)
  (append (to-list (add-indent
                    indent
                    (string-append (make-fn-to-first-arg (appexp-fn exp))
                                   (make-exp (first (appexp-args exp)) 0 width)
                                   ",")))
          (to-list (make-appexp-args (rest (appexp-args exp))
                                     (+ indent
                                        (get-symbol-length (appexp-fn exp))
                                        1)
                                     width))))

;; TEST :
(begin-for-test
  (check-equal? (make-fn-with-first-arg (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y))) 1 2)
                (list " f2(z," "    y)") "make-fnfirt args test fail"))

;; make-fn-to-first-arg : Variable -> String
;; GIVEN:A Variable fn 
;; RETURN: a string contains fn and left parenthesis
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (make-fn-to-first-arg fn)
  (string-append (symbol->string fn)
                 "("))
;; TEST :
(begin-for-test
  (check-equal? (make-fn-to-first-arg 'f1) "f1("))

;; make-appexp-args : ListOfExp NonNegInt PosInt -> ListOfString
;; GIVEN: A ListOfExp loe , a NonNegInt indent and a PosInt width
;; WHERE: the loe should be presented on seperate lines
;; RETURN: the presentation of loe with given indent
;; EXAMPLE: see in tests
;; DESIGN STRATEGY:use template of loe
(define (make-appexp-args loe indent width)
  (cond
    [(empty? loe) empty]
    [(and (< (length loe) 2)
          (varexp? (first loe)))
     (to-list (string-append (to-string (make-exp (first loe) indent width))
                             ")"))]
    [(and (< (length loe) 2)
          (appexp? (first loe)))
     (append (without-last (to-list (make-exp (first loe) indent width)))
             (to-list (string-append (last (to-list (make-exp (first loe) indent width)))
                                     ")")))]
    [else
     (append (to-list (string-append (to-string (make-exp (first loe) indent width))
                                     ","))
             (make-appexp-args (rest loe) indent width))]))

;; TEST : 




;; make-fn-diff-args : Exp NonNegInt PosInt -> ListOfString
;; GIVEN: A Exp exp, a NonNegInt indent and a PosInt width
;; WHERE: the exp should be presented that the function name is placed on a line
;;        by itself and arguments are placed on seperate lines
;; RETURN: the presentation of exp
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of appexp
(define (make-fn-diff-args exp indent width)
  (append (make-fn exp indent)
          (to-list
           (add-indent (+ indent 1)
                       (string-append
                        "("
                        (first (make-appexp-args (appexp-args exp)
                                                 0
                                                 width)))))
          (make-appexp-args (rest (appexp-args exp))
                            (+ 2 indent)
                            width)))

;; TEST :
(begin-for-test
  (check-equal? (make-fn-diff-args (make-appexp 'f1 (list (make-varexp 'x))) 1 5)
                '(" f1" "  (x)")
                "make-fn-diff-args test fail"))
                



;; make-fn : exp indent -> ListOfString
;; GIVEN: A Exp exp and a NonNegInt indent
;; WHERE: the function name of given exp should be presented on a single line
;; RETURN: A list of string contains function name of exp with indent
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of appexp
(define (make-fn exp indent)
  (list (add-indent indent (symbol->string (appexp-fn exp)))))

;; TEST :
(begin-for-test
  (check-equal? (make-fn (make-appexp 'f1 (list (make-varexp 'x))) 2)
                (list "  f1") "make-fn test fail"))



;; without-last : List -> List
;; GIVEN: A List lst
;; RETURN: A new list contains all elements in given list without the last one
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of list
(define (without-last lst)
  (cond
    [(< (length lst) 2)
     empty]
    [else (cons (first lst)
                (without-last (rest lst)))]))

;; TEST :
(begin-for-test
  (check-equal? (without-last (list 1 2 3))
                (list 1 2) "without last test fail"))



;; last : List -> List
;; GIVEN: A List lst
;; RETURN: the last element in the given list
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of list
(define (last lst)
  (cond
    [(< (length lst) 2)
     (first lst)]
    [else (last (rest lst))]))


;; TESTS :
(begin-for-test
  (check-equal? (last (list 1 2 3)) 3
                "last for list test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-one-line-exp: Exp -> String
;; GIVEN: A Exp exp
;; WHERE: The given exp is appexp and can be presented on single line 
;; RETURN: The string of presentation of the given exp
;; EXAMPLE:see in tests
;; DESIGN STRATEGY: use template of appexp
(define (make-one-line-exp exp)
  (string-append (symbol->string (appexp-fn exp))
                 "("
                 (make-one-line-args (appexp-args exp))
                 ")"))
;; TEST :
(begin-for-test
  (check-equal? (make-one-line-exp (make-appexp 'f1 (list (make-varexp 'x))))
                "f1(x)" "make-one-line emp test fail"))


;; make-one-line-args : ListOfExp -> String
;; GIVEN: A ListOfExp args
;; WHERE: the exp contains given args should be presented on a single line
;; RETURN: A string of given args' presentation
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of ListOfExp
(define (make-one-line-args args)
  (cond
    [(and (equal? (length args) 1)
          (varexp? (first args)))
     (symbol->string (varexp-name (first args)))]
    [(and (equal? (length args) 1)
          (appexp? (first args)))
     (make-one-line-exp (first args))]
    [(varexp? (first args))
     (string-append (symbol->string (varexp-name (first args)))
                    ", "
                    (make-one-line-args (rest args)))]
    [else
     (string-append (make-one-line-exp (first args))
                    ", "
                    (make-one-line-args (rest args)))]))

;; TEST :
(define LOE1 (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z)))))

(begin-for-test
  (check-equal? (make-one-line-args LOE1)
                "f2(z, y), z, f1(f2(z, y), z)" "make-one-line args test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-length-to-first-arg : Exp -> PosInt
;; GIVEN: A Exp exp
;; WHERE: the given exp is an appexp
;; RETURN: The length of fn to the first arg of this exp
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of appexp
(define (get-length-to-first-arg exp)
  (+ (get-symbol-length (appexp-fn exp))
     2
     (get-length-of-exp (first (appexp-args exp)))))

;; TEST :
(begin-for-test
  (check-equal? (get-length-to-first-arg
                 (make-appexp 'x (list (make-appexp 'x empty))))
                 4 "length of first arg fail"))



;; get-length-of-exp : Exp -> PosInt
;; GIVEN: A Exp exp
;; RETURN: The length of given exp
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of exp
(define (get-length-of-exp exp)
  (cond
    [(varexp? exp)
     (get-symbol-length (varexp-name exp))]
    [else
     (get-length-of-appexp exp)]))

;; TEST :
(begin-for-test
  (check-equal? (get-length-of-exp (make-appexp 'x empty)) 1
                "length of exp test 1 fail")
  (check-equal? (get-length-of-exp (make-varexp 'x)) 1
                "length of exp test 2 fail"))


;; get-length-of-appexp : Exp -> PosInt
;; GIVEN: A Exp exp
;; WHERE: the given exp is an Appexp
;; RETURN: the length of given appexp
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use template of appexp
(define (get-length-of-appexp exp)
  (+ (get-symbol-length (appexp-fn exp))
     (get-length-of-args (appexp-args exp))
     (* 2 (length (appexp-args exp)))))

;; TEST :
(begin-for-test
  (check-equal? (get-length-of-appexp (make-appexp 'x empty)) 1
                "length of apexp test fail"))


;; get-length-of-args : ListOfExp -> PosInt
;; GIVEN: A ListOfExp args
;; RETURN: The sum of the length of all exps in args
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: use HOF foldr 
(define (get-length-of-args args)
  (local (;; add-length : Exp Int -> Int
          ;; RETURN: the sum of exp's length and n
          (define (add-length exp n)
            (if (varexp? exp)
             (+ n
                (get-symbol-length (varexp-name exp)))
             (+ n
                (get-length-of-appexp exp)))))
          (foldr add-length 0 args)))

;; TEST :

(begin-for-test
  (check-equal? (get-length-of-args (list (make-varexp 'x))) 1
                "length of args test 1 fail")
  (check-equal? (get-length-of-args (list (make-appexp 'x empty))) 1
                "length of args test 2 fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-indent : NonNegInt -> String
;; GIVEN: A NonNegInt n
;; RETURN: A String contains space with length n
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (make-indent n)
  (make-string n #\ ))



;; add-indent : NonNegInt String -> String
;; GIVEN: A NonNegInt n and a String string
;; RETURN: A new string with indent of n spaces before given string
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (add-indent n string)
  (string-append (make-indent n)
                 string))

;; TESTS:
(begin-for-test
  (check-equal? (add-indent 2 "abc") "  abc"
                "add indent test fail"))



;; get-symbol-length : Symbol -> PosInt
;; GIVEN: A symbol s
;; RETURN: The length of this symbol
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (get-symbol-length s)
  (string-length (symbol->string s)))

;; TESTS :
(begin-for-test
  (check-equal? (get-symbol-length 'xyz) 3
                "symbol length test fail"))



;; to-string : String/ListOfString -> String
;; GIVEN: A String or a list of string s
;; WHERE: the length of given list of string is 1
;; RETURN: A string contains the context in the list
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (to-string s)
  (cond
    [(string? s) s]
    [(list? s) (first s)]))

;; TESTS :
(begin-for-test
  (check-equal? (to-string "f1 (x)") "f1 (x)"
                " to-string test 1 fail")
  (check-equal? (to-string (list "f1 (x)")) "f1 (x)"
                " to-string test 2 fail"))



;; to-list : String/ListOfString -> ListOfString
;; GIVEN: A string or a los 
;; WHERE: The length of los is 1
;; RETURN: A list of string
;; EXAMPLE: see in tests
;; DESIGN STRATEGY: combine simpler functions
(define (to-list s)
  (cond
    [(string? s) (list s)]
    [(list? s) s]))

;; TEST :
(begin-for-test
  (check-equal? (to-list "f1 (x)") (list "f1 (x)")
                " to-list test fail")
  (check-equal? (to-list (list "f1 (x)")) (list "f1 (x)")
                " to-list test fail"))