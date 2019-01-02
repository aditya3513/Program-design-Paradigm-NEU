;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(check-location "08" "q1.rkt")


(provide
 make-def
 make-varexp
 make-appexp
 any-loops?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Program is a ListOfDefinition.

;; TEMPLATE :
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

;; TEMPLATE :
;; def-fn : Definition -> ??
;; (define (def-fn def)
;;   (...(def-name def)
;;       (def-args def)
;;       (def-body def)))

(define-struct varexp (name))
;; A Varexp is a (make-varexp Variable)
;; INTERPRETATION :
;; name is the name of the function being defined

;; TEMPLATE :
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn varexp)
;;   (...(varexp-name varexp)))


(define-struct appexp (fn args))
;; A Appexp is a (make-appexp Variable ListOfExp)
;; INTERPRETATION :
;; fn is the body of the function.
;; args is the list of arguments of the function.

;; TEMPLATE :
;; appexp-fn : Appexp -> ??
;; (define (appexp-fn appexp)
;;   (...(appexp-fn varexp)
;;       (appexp-args varexp)))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE :
;; exp-fn : Exp -> ??
;; (define (exp-fn exp)
;;    (cond
;;       [(varexp? exp) (varexp-fn exp)]
;;       [else (appexp-fn exp)]))

;; A Variable is a Symbol.

(define-struct tree(node children))
;; A Tree is a (make-tree Symbol ListOfSymbol)
;; INTERPRETATION :
;; node is the Symbol at the root of the tree.
;; children is the ListOfSymbol that are children of this node.

;; TEMPLATE :
;; tree-fn : Tree -> ??
;(define (tree-fn t)
;  (... (tree-node t) (tree-children t) ))

;; A listofTree is either :
;; - empty
;; (cons Tree LOT)

;; TEMPLATE :
;; listoftree-fn : LOT -> ??
;; (define (listoftree-fn lot)
;;    (cond
;;       [(empty? lot) lot]
;;       [else (cons (tree-fn (first lot))
;;                   (listoftree-fn(rest lot))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define TREE-arg1 (list
 (make-tree
  'f4
  (list
   (make-tree
    'f5
    (list
     (make-tree 'f6 (list
                     (make-tree 'f4
                     (list (make-tree 'f5 (list '() '()))))
                     (make-tree 'f3 (list (make-tree 'f1 (list '()))
                     (make-tree 'f4 (list '()))))))
     (make-tree 'f1 (list (make-tree 'no-loop '())))))))))

(define TREE-2 (make-tree
    'f5
    (list
     (make-tree 'f6 (list
                     (make-tree 'f4
                     (list (make-tree 'f5 (list '() '()))))
                     (make-tree 'f3 (list (make-tree 'f1 (list '()))
                     (make-tree 'f4 (list '()))))))
     (make-tree 'f1 (list (make-tree 'no-loop '()))))))

(define arg1 (make-appexp 'f4 (list (make-varexp 'u)
                                                   (make-varexp 'w))))
(define arg2 (make-varexp 'x))

(define DEF1 (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x)))))
(define DEF2 (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y)))))
(define DEF3 (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f4
                                             (list (make-varexp 'u)
                                                   (make-varexp 'w)))
                                      (make-varexp 'z)))))
(define DEF4 (make-def 'f4 (list 'x 'y)
               (make-appexp 'f5
                            (list (make-varexp 'y)
                                  (make-varexp 'u)))))
(define DEF5 (make-def 'f5 (list 'u)
               (make-appexp 'f6
                            (list (make-appexp 'f1 empty)))))
(define DEF6 (make-def 'f6 (list 'u)
               (make-appexp 'f4
                            (list (make-appexp 'f3 empty)))))
(define DEF7 (make-def 'no-loop (list 'x) (make-varexp 'x)))

(define SOME-LOOPS
  (list DEF1 DEF2 DEF3 DEF4 DEF5 DEF6 DEF7))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make-stress-input-without-loops : PosInt -> Program
;;; GIVEN: an integer n
;;; RETURNS: an SGS program with no loops that defines n functions
;;; EXAMPLES:
;;;     (make-stress-input-without-loops 1)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
;;;
;;;     (make-stress-input-without-loops 3)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x))
;;;      (make-def 'f2 (list 'x 'y)
;;;        (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
;;;      (make-def 'f3 (list 'x 'y)
;;;        (make-appexp
;;;         'f2
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x)))
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x))))))

(define (make-stress-input-without-loops n)
  (local (
          ;;; Returns a list of 1 through k.
          (define (iota k)
            (reverse (reverse-iota k)))
          (define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))

          ;;; Given the function names in reverse order,
          ;;; returns their bodies in reverse order.
          (define (make-bodies names)
            (if (empty? (rest names))
                (list (make-varexp 'x))
                (let* ((bodies (make-bodies (rest names)))
                       (body (first bodies))
                       (name (first (rest names))))
                  (cons (make-appexp name (list body body))
                        bodies)))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "f"
                                                       (number->string k))))
                      nums))
           (bodies (reverse (make-bodies (reverse syms)))))
      (map (lambda (sym body)
             (make-def sym (list 'x 'y) body))
           syms
           bodies))))

;; TESTS :
(begin-for-test
  (check-equal? (make-stress-input-without-loops 1)
                (list
                 (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
                "make-stress-input-without-loops test fail")

  (check-equal? (make-stress-input-without-loops 3)
                (list
                 (make-def 'f1 (list 'x 'y) (make-varexp 'x))
                 (make-def 'f2 (list 'x 'y)
                           (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
                 (make-def 'f3 (list 'x 'y)
                           (make-appexp
                            'f2
                            (list (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x)))
                                  (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x)))))))
                "make-stress-input-without-loops test fail"))


;;; stress-benchmark1 : PosInt -> Boolean
;;; GIVEN: a positive integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-without-loops n) has no loops

(define (stress-benchmark1 n)
  (time (any-loops? (make-stress-input-without-loops n))))

;; TESTS :
(begin-for-test
  (check-false (stress-benchmark1 5)
              "stress-benchmark1 test failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;;        restrictions listed above).
;; RETURNS: true iff there is some function f in p that calls itself
;; either directly or indirectly, as in the example above.
;; DESIGN STRATEGY : using HOF ormap on program
;; EXAMPLES :
;; (any-loops? some-loops) = true

(define (any-loops? program)
  (ormap
   (lambda (d) (exist-loop-of-def? d program))
   program))

;; TESTS :

(begin-for-test
  (check-equal? (any-loops? SOME-LOOPS) true
                "any-loops? test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exist-same-function-in-children : ListOfTree Symbol -> Boolean
;; GIVEN: children of the tree and function
;; RETURN: true if same function exits in the tree
;; EXAMPLES:
;;(exist-same-function-in-children TREE-2 'f1) = true
;; HALTING MEASURE: length of children
;; DESIGN STRATEGY: use HOF ormap on children

(define (exist-same-function-in-children children fn)
  (ormap
   (lambda (t) (exist-same-function-in-tree t fn))
   children))

;; TESTS :
(begin-for-test
  (check-true (exist-same-function-in-children TREE-arg1 'f1)
              "exist-same-function-in-children test failed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exist-same-function-in-tree : Tree Symbol -> Boolean
;; GIVEN: A tree and a symbol
;; RETURN: true if symbol exits in given tree else false
;; EXAMPLES:
;; (exist-same-function-in-tree TREE-2 'f9) = false
;; (exist-same-function-in-tree TREE-2 'f1) = true
;; DESIGN STRATEGY: use template of tree
(define (exist-same-function-in-tree tree fn)
  (cond
    [(empty? tree) #f]
    [(equal? (tree-node tree) fn) #t]
    [else (exist-same-function-in-children (tree-children tree) fn)]))

;; TESTS : 
(begin-for-test
  (check-true (exist-same-function-in-tree TREE-2 'f1)
              "exist-same-function-in-tree test 1 failed")
  (check-false (exist-same-function-in-tree TREE-2 'f9)
              "exist-same-function-in-tree test 2 failed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; get-functions-in-loe : LOE -> LOF
;; GIVEN: A list of exp
;; RETURN: a list of functions called in loe
;; EXAMPLES:
;; (get-functions-in-loe (list (make-varexp 'x))) = empty
;; DESIGN STRATEGY: divinf on cases by value of args
;; HALTING MEASURE :  length of args i.e the LOE
(define (get-functions-in-loe args)
  (cond
    [(empty? args) empty]
    [(varexp? (first args))
     (get-functions-in-loe (rest args))]
    [else
     (cons (appexp-fn (first args))
           (get-functions-in-loe (rest args)))]))

;;TESTS :
(begin-for-test
  (check-equal? (get-functions-in-loe (list (make-varexp 'x)))
                empty "get-functions-in-loe test failed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-all-function-called : Appexp -> LOF
;; GIVEN: an appexp
;; RETURN: all functions called in this appexp
;; EXAMPLES:
;; (get-all-function-called (make-appexp 'no-loop (list (make-varexp 'x))))
;; => (list 'no-loop)
;; DESIGN STRATEGY: combining simpler functions

(define (get-all-function-called appexp)
  (cons (appexp-fn appexp)
        (get-functions-in-loe (appexp-args appexp))))

;; TESTS:

(begin-for-test
  (check-equal?
   (get-all-function-called (make-appexp 'no-loop (list (make-varexp 'x))))
   (list 'no-loop) "get-all-function-called test failed"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-def-with-name : String Program -> Def
;; GIVEN: name of the defination to search in the program
;;        and prgram itself
;; RETURN: defination matching with the given name
;; EXAMPLES:
;; (get-def-with-name 'f1 SOME-LOOPS) = DEF1
;; DESIGN STRATEGY: divding on cases by value of program and name
;; HALTING MEASURE : length of program

(define (get-def-with-name name program)
  (cond
    [(empty? program) empty]
    [(equal? name (def-name (first program)))
     (first program)]
    [else (get-def-with-name name (rest program))]))

;; TESTS :
(begin-for-test
  (check-equal? (get-def-with-name 'f1 SOME-LOOPS) DEF1
                "test case 1 for get-def-with-name failed")
  (check-equal? (get-def-with-name 'f1 empty) empty
                "test case 1 for get-def-with-name failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; get-call-tree-of-def : Definition Program NonNegInt -> Tree
;; GIVEN: A defination, A program and a depth as in length of program
;; RETURN: A tree with node as defination and exp as its children
;; EXAMPLES:
;; (get-call-tree-of-def DEF1 SOME-LOOPS 1) = TREE-DEF1
;; DESIGN STRATEGY: divide on cases by value of def
(define (get-call-tree-of-def def program depth)
  (cond
    [(or (empty? def)
         (> depth (length program)))
     empty]
    [else
     (make-tree (def-name def)
                (get-tree-children-of-exp (def-body def) program (+ depth 1)))]))

;; TESTS :

(define TREE-DEF1 (make-tree 'f1 (list (make-tree 'no-loop '()))))

(begin-for-test
  (check-equal? (get-call-tree-of-def DEF1 SOME-LOOPS 1) TREE-DEF1
                "get-call-tree-of-def test 1 failed")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-tree-children-of-exp : Exp Program NonNegInt -> LOT
;; GIVEN: an exp, program and depth for the tree
;; RETURN: a list of trees with children of exp
;; EXAMPLES:
;; (get-tree-children-of-exp arg1 SOME-LOOPS 3) = TREE-arg1
;; DESIGN STRATEGY:
(define (get-tree-children-of-exp exp program depth)
  (cond
    [(varexp? exp) empty]
    [else (get-call-tree-lst-of-los
           (get-all-function-called exp) program depth)]))

;; TESTS :


(begin-for-test
  (check-equal? (get-tree-children-of-exp arg1 SOME-LOOPS 3)
                TREE-arg1 "get-tree-children-of-exp test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; exist-loop-of-def? : Definition Program -> Boolean
;; GIVEN: A Defination and a Program
;; RETURN: true if loop exits in defination else false
;; EXAMPLES:
;; (exist-loop-of-def? DEF2 SOME-LOOPS) = false
;; (exist-loop-of-def? DEF3 SOME-LOOPS) = true
;; DESIGN STRATEGY:
(define (exist-loop-of-def? def program)
  (exist-same-function-in-children
   (tree-children (get-call-tree-of-def def program 0))
   (def-name def)))

;; TESTS :
(begin-for-test
  (check-equal? (exist-loop-of-def? DEF2 SOME-LOOPS) false
                "exist-loop-of-def? test 1 failed")
  (check-equal? (exist-loop-of-def? DEF3 SOME-LOOPS) true
                "exist-loop-of-def? test 2 failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; get-call-tree-lst-of-los : ListofSymbols Program NonNegInt -> Tree
;; GIVEN: a LOS, program and depth for tree.
;; RETURN: a tree with list of symbols in it.
;; EXAMPLES:
;; (get-call-tree-lst-of-los TREE-arg1 SOME-LOOPS 3) = (list empty)
;; DESIGN STRATEGY:
(define (get-call-tree-lst-of-los los program depth)
  (cond
    [(empty? los) empty]
    [else (cons (get-call-tree-of-def (get-def-with-name (first los) program)
                                      program depth)
                (get-call-tree-lst-of-los (rest los) program depth))]))

;; TESTS :
(begin-for-test
  (check-equal? (get-call-tree-lst-of-los TREE-arg1 SOME-LOOPS 3) (list empty)
                " get-call-tree-lst-of-los test fail" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;