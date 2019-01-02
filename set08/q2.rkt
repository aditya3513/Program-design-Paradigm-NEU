;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(check-location "08" "q2.rkt")

(provide
 is-null-derivable?
 make-pos
 make-neg
 make-clause)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

;; A Variable is a Racket Symbol.

;; A Literal is one of
;; -- (make-pos Variable)  Interp: a literal containing the variable
;; -- (make-neg Variable)  Interp: a literal containing the negation of
;;                                 the variable
;; template:
;; Literal-fn : Literal -> ??
;; (define (literal-fn l)
;;   (cond
;;     ...
;;     [(pos? l) (pof-fn l)]
;;     [(neg? l) (neg-fn l)]))

(define-struct pos(var))
;; A Pos is a (make-pos var)
;; var is the variable of this pos-literal
;; template:
;; Pos-fn : Pos -> ??
;; (define (pos-fn p)
;;   (...
;;    (pos-var p)))

(define-struct neg(var))
;; A Neg is a (make-neg var)
;; var is the variable of this neg-literal
;; template:
;; Neg-fn : Neg -> ??
;; (define (neg-fn p)
;;   (...
;;    (neg-var p)))

(define-struct clause(lol))
;; A Clause is a SetOfLiteral
;; template:
;; clause-fn : Clause -> ??
;; (define (clause-fn c)
;;   (...
;;    (clause-lol c)))

;; A SetOfLiteral is A ListOfLiteral without duplication
;; A SetOfLiteral is one of
;; -- empty
;; -- (cons literal SetOfLiteral)
;; template:
;; SetOfLiteral-fn : SetOfLiteral -> ??
;; HALTING MEASURE: length of sol
;; (define (sol-fn sol)
;;   (cond
;;     [(empty? sol) ...]
;;     [else (cons (literal-fn (first sol))
;;                 (sol-fn (rest sol)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample for test:
(define EMPTY-CLAUSE (make-clause empty))

(define -A (make-neg 'a))
(define -B (make-neg 'b))
(define -C (make-neg 'c))
(define -D (make-neg 'd))

(define A (make-pos 'a))
(define B (make-pos 'b))
(define C (make-pos 'c))
(define D (make-pos 'd))

(define CLAUSE-1 (make-clause (list -A -B -C)))
(define CLAUSE-2 (make-clause (list A -B -C)))
(define CLAUSE-3 (make-clause (list -A B -C)))
(define CLAUSE-4 (make-clause (list -A -B C)))

(define LOC-1 (list CLAUSE-1 CLAUSE-2))
(define LOC-2 (list CLAUSE-1 CLAUSE-3 CLAUSE-4))

(define SOL-1 (list -A -B -C))
(define SOL-1-DUPS (list -A -B -C -C))

(define TEST-LOC (list CLAUSE-1 CLAUSE-2 CLAUSE-3 CLAUSE-4))

(define CLAUSE-5 (make-clause (list A -B C C)))
(define CLAUSE-6 (make-clause (list -A C)))
(define CLAUSE-7 (make-clause (list -B D)))
(define CLAUSE-8 (make-clause (list B)))
(define CLAUSE-9 (make-clause (list -C)))


(define DERIVABLE-LOC (list CLAUSE-5 CLAUSE-6 CLAUSE-7
                            CLAUSE-8 CLAUSE-9))

(define C1 (make-clause (list A -B C)))
(define C2 (make-clause (list D B)))
(define C3 (make-clause (list -A C)))
(define C4 (make-clause (list B)))
(define C5 (make-clause (list -C)))

(define D-LOC (list C1 C2 C3 C4 C5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;;          clauses using the rule of resolution.
;; DESIGN STRATEGY : divide on cases by value of loc

(define (is-null-derivable? loc)
  (cond
    [(my-member? EMPTY-CLAUSE loc) #t]
    [(empty? loc) #f]
    [else (is-loc-derivable? loc)]))

;; TESTS :

(define LOC-with-EMPTY (cons EMPTY-CLAUSE LOC-1))

(begin-for-test
  (check-true (is-null-derivable? DERIVABLE-LOC)
              "is-null-derivable? test 1 failed")
  (check-false (is-null-derivable? TEST-LOC)
              "is-null-derivable? test 2 failed")
  (check-false (is-null-derivable? empty)
              "is-null-derivable? test 3 failed")
  (check-true (is-null-derivable? LOC-with-EMPTY)
              "is-null-derivable? test 4 failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-all-combinations-of-loc : ListOfClause NonNegInt -> ListOfLOC
;; GIVEN: A list of clause and a NonNegInt n
;; WHERE: n is the length of combinations of loc currently getting
;; RETURN: A list of loc contains all possible combinations of clauses
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: combine simpler functions
(define (get-all-combinations-of-loc loc n)
  (if (> n (length loc))
      empty
      (append (get-all-combinations n loc)
              (get-all-combinations-of-loc loc (+ n 1)))))

;; TESTS :
(begin-for-test
  (check-equal? (get-all-combinations-of-loc TEST-LOC 4)
                (list
                 (list
                  (make-clause
                   (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                  (make-clause
                   (list (make-pos 'a) (make-neg 'b) (make-neg 'c)))
                  (make-clause
                   (list (make-neg 'a) (make-pos 'b) (make-neg 'c)))
                  (make-clause
                   (list
                    (make-neg 'a)
                    (make-neg 'b)
                    (make-pos 'c))))) "get-all-combinations-of-loc test failed"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-all-combinations : Int ListOfClause -> ListOfLOC
;; GIVEN: A int size and a ListOfClause loc
;; WHERE: the number of clauses in the every possible combinations is size
;; RETURN: All possible combinations of clauses in the loc 
;; EXAMPLES: see below in tests
;; HALTING MEASURE: length of loc
;; DESIGN STRATEGY: use template of loc
(define (get-all-combinations size loc)
  (cond
    [(equal? size 0)
     (list empty)]
    [(empty? loc)
     empty]
    [else
     (append
      (get-combinations loc (get-all-combinations (sub1 size) (rest loc)))
      (get-all-combinations size (rest loc)))]))

;; TESTS :

(begin-for-test
  (check-equal? (get-all-combinations 4 TEST-LOC)
                (list
                 (list
                  (make-clause
                   (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                  (make-clause
                   (list (make-pos 'a) (make-neg 'b) (make-neg 'c)))
                  (make-clause
                   (list (make-neg 'a) (make-pos 'b) (make-neg 'c)))
                  (make-clause
                   (list
                    (make-neg 'a)
                    (make-neg 'b)
                    (make-pos 'c)))))
                "get-all-combinations test failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-combinations : ListOfClause ListOfLOC -> ListOfClause
;; GIVEN: A list of clause loc and a list of loc c
;; RETURN: A list of loc contains all possible combinations with first elements of loc
;; EXAMPLES: see below in tests
;; HALTING MEASURE: length of ListOfClause
;; DESIGN STRATEGY:use template of loc
(define (get-combinations loc c)
  (cond
    [(empty? c) empty]
    [else
     (cons (cons (first loc) (first c))
           (get-combinations loc (rest c)))]))

;; TESTS :

(begin-for-test
  (check-equal? (get-combinations LOC-1 (list LOC-1 LOC-2))
                (list (list (make-clause (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                            (make-clause (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                            (make-clause (list (make-pos 'a) (make-neg 'b) (make-neg 'c))))
                      (list (make-clause (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                            (make-clause (list (make-neg 'a) (make-neg 'b) (make-neg 'c)))
                            (make-clause (list (make-neg 'a) (make-pos 'b) (make-neg 'c)))
                            (make-clause (list (make-neg 'a) (make-neg 'b) (make-pos 'c)))))
                "get-combinations test failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; invert : Literal -> Literal
;; GIVEN: A literal l
;; WHERE: literal may be positive or negative
;; RETURN: negation of original literal 
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use template of literal

(define (invert l)
  (cond
    [(pos? l) (make-neg (pos-var l))]
    [else (make-pos (neg-var l))]))

;; TESTS :
(begin-for-test
  (check-equal? (invert A) -A
                "invert test case 1 fail")
  (check-equal? (invert -A) A
                "invert test case 2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-complementary? : Literal Literal -> Boolean
;; GIVEN: two literals l1 and l2
;; RETURN: true iff the two given literals have same variable and opposite type
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: combining simpler function
(define (is-complementary? l1 l2)
  (equal? l2 (invert l1)))

;; TESTS :

(begin-for-test
  (check-equal? (is-complementary? A A) false
                "is-complementary test fail")
  (check-equal? (is-complementary? -A A) true
                "is-complementary test fail")
  (check-equal? (is-complementary? -B A) false
                "is-complementary test fail")
  (check-equal? (is-complementary? B A) false
                "is-complementary test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; clause-sol : Clause -> SetOfLiteral
;; GIVEN: a clause c
;; WHERE: the list of literal of c has possibly duplication
;; RETURN: a set of literal without duplication
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY:use template of clause
(define (clause-sol c)
  (check-duplication (clause-lol c)))

;; TESTS :
(begin-for-test
  (check-equal? (clause-sol CLAUSE-1)
                SOL-1 "clause-sol test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exist-same-literal? : Literal SetOfLiteral -> Boolean
;; GIVEN: a literal l and a set of literal sol
;; RETURN: true iff there exist same literal as l in sol
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF ormap on sol
(define (exist-same-literal? l sol)
  (ormap
   (lambda (n) (equal? l n))
   sol))

;; TESTS :
(begin-for-test
  (check-true (exist-same-literal? -A SOL-1)
              "exist-same-literal? test 1 failed")
  (check-false (exist-same-literal? A SOL-1)
              "exist-same-literal? test 1 failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                

;; exist-complementary-in-clause? : Literal Clause -> Boolean
;; GIVEN: A literal l and a clause c
;; RETURN: true iff there exist complementary literal with l in c
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF ormap on clause-sol
(define (exist-complementary-in-clause? l c)
  (ormap
   (lambda (n) (is-complementary? l n))
   (clause-sol c)))

;; TESTS :
(begin-for-test
  (check-false (exist-complementary-in-clause? -A CLAUSE-1)
              "exist-complementary-in-clause? test 1 failed")
  (check-true (exist-complementary-in-clause? A CLAUSE-1)
              "exist-complementary-in-clause? test 2 failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-duplication : ListOfLiteral -> ListOfLiteral
;; GIVEN: a list of literal 
;; RETURN: a set of literal without duplication
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use template of sol
(define (check-duplication sol)
  (cond
    [(empty? sol) empty]
    [(exist-same-literal? (first sol) (rest sol))
     (check-duplication (rest sol))]
    [else
     (cons (first sol)
           (check-duplication (rest sol)))]))

;; TESTS :
(begin-for-test
  (check-equal? (check-duplication SOL-1-DUPS)
                SOL-1 "check-duplication test case failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-number-of-complementary : Clause Clause NonNegInt -> NonNegInt
;; GIVEN: two clause c1, c2 and a NonNegInt n
;; RETURN: add n with the number of complementary literal between c1 and c2
;; WHERE : initial value of n is zero and is updated at every all by adding 1 to it
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF foldr
(define (get-number-of-complementary c1 c2 n)
  (foldr
   (lambda (l n)
     (if (exist-complementary-in-clause? l c2)
         (+ n 1)
         n))
   n (clause-sol c1)))

;; TESTS :
(begin-for-test
  (check-equal? (get-number-of-complementary CLAUSE-1 CLAUSE-2 0)
                1 "get-number-of-complementary test failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get-complementary-clause : Clause ListOfClause -> Clause
;; GIVEN: A clause c and a list of clause loc
;; RETURN: true iff c has empty list of literal, false iff loc is empty,
;;         else return the clause has one complementary literal with c
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use template of loc
(define (get-complementary-clause c loc)
   (cond
     [(empty? (clause-sol c)) #t]
     [(empty? loc) #f]
     [(equal? (get-number-of-complementary c (first loc) 0) 1)
      (first loc)]
     [else (get-complementary-clause c (rest loc))]))


;; TESTS

(begin-for-test
  (check-equal? (get-complementary-clause CLAUSE-1 TEST-LOC)
                (make-clause (list A -B -C))
                "get-complementary-clause test 1 fail")
  (check-equal? (get-complementary-clause EMPTY-CLAUSE TEST-LOC)
                true
                "get-complementary-clause test 2 fail")
  (check-equal? (get-complementary-clause CLAUSE-1 empty)
                false
                "get-complementary-clause test 3 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-rest-loc : ListOfClause -> ListOfClause
;; GIVEN: A list of clause loc
;; WHERE: existing clause has one complementary variable with first of loc
;; RETURN: a list of clause as given one without the first and complementary element
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use template of loc
(define (get-rest-loc loc)
  (set-diff loc
            (list (first loc)
                  (get-complementary-clause (first loc) (rest loc)))))

;; TESTS :

(define OP-LOC (list (make-clause (list -A B -C))
      (make-clause (list -A -B C))))

(begin-for-test
  (check-equal? (get-rest-loc TEST-LOC) OP-LOC
                "get-rest-loc test faled"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exist-derivable-clause? : Clause ListOfClause -> Boolean
;; GIVEN: A clause and a list of clause loc
;; RETURN: true iff there exist a clause has one complementary literal with given clause
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF ormap on loc
(define (exist-derivable-clause? clause loc)
  (ormap
   (lambda (c) (if (equal? (get-number-of-complementary clause c 0) 1)
                   #t
                   #f))
   loc))

;; TESTS :

(begin-for-test
  (check-true (exist-derivable-clause? CLAUSE-1 TEST-LOC)
              "exist-derivable-clause? test case failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; delete-complementary : Clause Clause -> ListOfLiteral
;; GIVEN: Two clause c1 and c2
;; RETURN: a list of literal in c1 without complementary literal in c2
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF filter on clause-sol
(define (delete-complementary c1 c2)
  (filter
   (lambda (l) (not (exist-complementary-in-clause? l c2)))
   (clause-sol c1)))


;; TESTS :
(define LOL-2-1 (list (make-neg 'b) (make-neg 'c)))

(begin-for-test
  (check-equal? (delete-complementary CLAUSE-2 CLAUSE-1)
                LOL-2-1 "delete-complementary test equal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; derive-two-clause : Clause Clause -> Clause
;; GIVEN: two clause c1 and c2
;; WHERE: there exist one complementary literal between c1 and c2
;; RETURN: A new clause after derive c1 and c2
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use template of clause
(define (derive-two-clause c1 c2)
  (make-clause (check-duplication (append (delete-complementary c1 c2)
                                          (delete-complementary c2 c1)))))

;; TESTS :
(define TEST-CLAUSE (make-clause (list (make-neg 'b) (make-neg 'c))))

(begin-for-test
  (check-equal? (derive-two-clause CLAUSE-1 CLAUSE-2)
                TEST-CLAUSE "derive-two-clause test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; derive-loc : ListOfClause -> ListOfClause/Boolean
;; GIVEN: A list of clause loc
;; RETURN: true if LOC if derivable
;;         until last state it gives the LOC in mid states
;; EXAMPLES: see below in tests
;; HALTING MEASURE: the value of c
;; DESIGN STRATEGY: use template of loc
(define (derive-loc loc)
  (local (
          (define c (get-complementary-clause (first loc) (rest loc))))
    (cond
      [(boolean? c) c]
      [else
       (derive-loc (append (list (derive-two-clause (first loc) c))
                           (get-rest-loc loc)))])))

;; TESTS :

(begin-for-test
  (check-false (derive-loc TEST-LOC) "derive-loc test 1 failed")
  (check-true (derive-loc DERIVABLE-LOC) "derive-loc test 2 failed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-loc-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; WHERE: the list of clauses is not empty nor contains empty clause
;; RETURN: true iff the empty clause is derivable from the given
;;         clauses using the rule of resolution.
;; EXAMPLES: see below in tests
;; DESIGN STRATEGY: use HOF ormap on loc

(define (is-loc-derivable? loc)
  (local ((define l (get-all-combinations-of-loc loc 1)))
    (ormap
     (lambda (lst) (derive-loc lst))
     l)))


;; TEST
(begin-for-test
  (check-equal? (is-loc-derivable? DERIVABLE-LOC)
                true  "is-loc-derivable? test 1 fail")
  (check-equal? (is-loc-derivable? TEST-LOC)
                false  "is-loc-derivable? test 2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-stress-input-sat : NonNegInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: a satisfiable set of clauses of length n
;;; EXAMPLES:
;;;     (make-stress-input-sat 0) => empty
;;;     (make-stress-input-sat 3)
;;;  => (list (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))



(define (make-stress-input-sat n)
  (local ((define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          (define (iota k)
            (reverse (reverse-iota k))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "p"
                                                       (number->string k))))
                      nums)))
      (map (lambda (k)
             (make-clause   ; see note above
              (map (lambda (i)
                     ((if (= i k) make-pos make-neg)
                      (list-ref syms (- i 1))))
                   nums)))
           nums))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; make-stress-input-unsat : PosInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: an unsatisfiable set of clauses of length 2n

(define (make-stress-input-unsat n)
  (local ((define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          (define (iota k)
            (reverse (reverse-iota k))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "p"
                                                       (number->string k))))
                      nums)))
      (cons (make-clause (list (make-neg (first syms))))
            (append
             (map (lambda (sym)
                    (make-clause (list (make-pos sym))))
                  (rest syms))
             (map (lambda (k)
                    (make-clause
                     (map (lambda (i)
                            ((if (= i k) make-pos make-neg)
                             (list-ref syms (- i 1))))
                          nums)))
                  nums))))))

;; TESTS :
(begin-for-test
  (check-equal? (make-stress-input-unsat 2)
                (list
 (make-clause (list (make-neg 'p1)))
 (make-clause (list (make-pos 'p2)))
 (make-clause (list (make-pos 'p1) (make-neg 'p2)))
 (make-clause (list (make-neg 'p1) (make-pos 'p2))))
                "make-stress-input-unsat test failed"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; stress-benchmark2 : NonNegInt -> Boolean
;;; GIVEN: a non-negative integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-sat n) is satisfiable
;; DESIGN STRATEGY : combine simpler functions

(define (stress-benchmark2 n)
  (time (is-null-derivable? (make-stress-input-sat n))))

;; TESTS :

(begin-for-test
  (check-false (stress-benchmark2 4)
               "stress-benchmark2 test case failed"))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;