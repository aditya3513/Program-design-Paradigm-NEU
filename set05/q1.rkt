;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "05" "q1.rkt")


(provide make-slip
         slip-color
         slip-name1
         slip-name2
         felleisen-roster
         shivers-roster
         possible-roster?
         acceptable-felleisen-answer?)


;; CONSTANTS :
(define COLOR1 "yellow")
(define COLOR2 "blue")

;;; DATA DEFINITIONS

;; A Color hs one of two values:
;; COLOR1 or COLOR2
;; Interpretation :
;; this represents the color of the slip

;-----------

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; Interpretation: 
;; color is Color.
;; name1 is the first name or last name of student.
;; name2 is the last name or first name of student.

;; template:
;; slip-fn : Slip -> ??
;(define (slip-fn s)
;  (... (slip-color s)
;       (slip-name1 s)
;       (slip-name2 s)))

;;examples of slip, for testing
(define slip1 (make-slip COLOR1 "abc" "def"))  
(define slip2 (make-slip COLOR2 "xyz" "pqr"))

;; Definations for test cases
(define n1 (make-slip COLOR1 "a" "b"))
(define n2 (make-slip COLOR1 "x" "y"))
(define n3 (make-slip COLOR1 "b" "a"))
(define n4 (make-slip COLOR1 "ab" "bz"))
(define n5 (make-slip COLOR1 "abc" "b"))
(define n6 (make-slip COLOR1 "a" "b"))
(define n7 (make-slip COLOR1 "a1" "b1"))

(define n11 (make-slip COLOR1 "a" "b"))
(define n12 (make-slip COLOR1 "x" "y"))
(define n13 (make-slip COLOR1 "b" "a"))
(define n14 (make-slip COLOR2 "ab" "bz"))
(define n15 (make-slip COLOR1 "abc" "b"))
(define n16 (make-slip COLOR1 "a" "b"))
(define n17 (make-slip COLOR2 "a1" "b1"))
(define n18 (make-slip COLOR2 "a1" "b1"))
(define n19 (make-slip COLOR2 "ab" "b1"))

(define CORRECT-FELL-LIST (list n1 n2 n4 n5 n7))
(define INCORRECT-FELL-LIST (list n1 n2 n3 n4 n5 n6 n7))
(define CORRECT-SHIV-LIST (list n14 n17 n19))
(define INCORRECT-SHIV-LIST (list n14 n17 n18 n19))
(define MIX-LIST (list n11 n12 n13 n14 n15 n16 n17 n18 n19))
(define YELLOW-LIST (list n11 n12 n13 n15 n16))
(define BLUE-LIST (list n14 n17 n18 n19))
(define MIX-FELL-OPT (list n11 n12 n15))
(define MIX-SHIV-OPT (list n14 n17 n19))

(define ACCEPTABLE-LIST (list
              (make-slip COLOR1 "Wang" "Xi")
              (make-slip COLOR2 "Jones" "Tom")
              (make-slip COLOR1 "Xi" "Wang")
              (make-slip COLOR1 "Shriram" "K.")))

(define CORRECT-INPUT1 (list
 (make-slip COLOR1 "Wang" "Xi")
 (make-slip COLOR1 "Shriram" "K.")))

(define CORRECT-INPUT2 (list
 (make-slip COLOR1 "Shriram" "K.")
 (make-slip COLOR1 "Wang" "Xi")))

(define CORRECT-INPUT3 (list
 (make-slip COLOR1 "Shriram" "K.")
 (make-slip COLOR1 "Xi" "Wang")))

(define CORRECT-INPUT4 (list
 (make-slip COLOR1 "K." "Shriram")
 (make-slip COLOR1 "Xi" "Wang")))

(define INCORRECT-INPUT1 (list
 (make-slip COLOR1 "K." "Shriram")
 (make-slip COLOR1 "Xi" "Wang")
 (make-slip COLOR1 "Xi" "Wang")))

(define INCORRECT-INPUT2 (list
 (make-slip COLOR1 "K." "Shriram")
 (make-slip COLOR1 "Shriram" "K.")
 (make-slip COLOR1 "Xi" "Wang")))

(define INCORRECT-INPUT3 (list
 (make-slip COLOR1 "K." "Shriram")
 (make-slip COLOR1 "Shriram" "K.")
 (make-slip COLOR2 "Jackie " "Chan")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Shivers' class, without duplication.

;; STRATEGY : combine simpler functions
;; EXAMPLES :
;; (shivers-roster CORRECT-SHIV-LIST) = CORRECT-SHIV-LIST
;; (shivers-roster INCORRECT-SHIV-LIST) = CORRECT-SHIV-LIST
;; (shivers-roster MIX-LIST) = MIX-SHIV-OPT

(define (shivers-roster lst)
  (remove-dups (return-list-with-color lst COLOR2)))


;; TESTS :
(begin-for-test
  (check-equal? (shivers-roster CORRECT-SHIV-LIST)
                CORRECT-SHIV-LIST "shivers-roster test 1 fail")
  (check-equal? (shivers-roster INCORRECT-SHIV-LIST)
                CORRECT-SHIV-LIST "shivers-roster test 2 fail")
  (check-equal? (shivers-roster MIX-LIST)
                MIX-SHIV-OPT "shivers-roster test 3 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Felleisen's class, without duplication.

;; STRATEGY : combinig simpler functions
;; EXAMPLES :
;; (felleisen-roster CORRECT-FELL-LIST) = CORRECT-FELL-LIST
;; (felleisen-roster INCORRECT-FELL-LIST) = CORRECT-FELL-LIST
;; (felleisen-roster MIX-LIST) = MIX-FELL-OPT

(define (felleisen-roster lst)
  (remove-dups (return-list-with-color lst COLOR1)))

;; TESTS :

(begin-for-test
  (check-equal? (felleisen-roster CORRECT-FELL-LIST)
                CORRECT-FELL-LIST "felleisen-roster test 1 fail")
  (check-equal? (felleisen-roster INCORRECT-FELL-LIST)
                CORRECT-FELL-LIST "felleisen-roster test 2 fail")
  (check-equal? (felleisen-roster MIX-LIST)
                MIX-FELL-OPT "felleisen-roster test 3 fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.

;; STRATEGY : combining simpler functions
;; EXAMPLES :
;; (possible-roster? CORRECT-FELL-LIST) = true
;; (possible-roster? MIX-LIST) = false
;; (possible-roster? CORRECT-SHIV-LIST) = true

(define (possible-roster? lst)
  (or
   (equal? (shivers-roster lst) lst)
   (equal? (felleisen-roster lst) lst)))


;; TESTS :

(begin-for-test
  (check-equal? (possible-roster? CORRECT-FELL-LIST)
                true "possible-roster? test 1 fail")
  (check-equal? (possible-roster? MIX-LIST)
                false "possible-roster? test 2 fail")
  (check-equal? (possible-roster? CORRECT-SHIV-LIST)
                true "possible-roster? test 3 fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;;          and only once in lst2.

;; STRATEGY : combining simpler functions
;; EXAMPLES :
;; (acceptable-felleisen-answer? ACCEPTABLE-LIST CORRECT-INPUT1) = true
;; (acceptable-felleisen-answer? ACCEPTABLE-LIST CORRECT-INPUT2) = true
;; (acceptable-felleisen-answer? ACCEPTABLE-LIST INCORRECT-INPUT1) = false
;; (acceptable-felleisen-answer? ACCEPTABLE-LIST INCORRECT-INPUT2) = false

(define (acceptable-felleisen-answer? lst1 lst2)
   (equal? (length (return-member-list (felleisen-roster lst1) lst2))
           (length lst2)))
  

;; TESTS :

(begin-for-test
  (check-equal? (acceptable-felleisen-answer? ACCEPTABLE-LIST CORRECT-INPUT1)
                true "acceptable-felleisen-answer? test 1 fail")
  (check-equal? (acceptable-felleisen-answer? ACCEPTABLE-LIST CORRECT-INPUT2)
                true "acceptable-felleisen-answer? test 2 fail")
  (check-equal? (acceptable-felleisen-answer? ACCEPTABLE-LIST INCORRECT-INPUT1)
                false "acceptable-felleisen-answer? test 3 fail")
  (check-equal? (acceptable-felleisen-answer? ACCEPTABLE-LIST INCORRECT-INPUT2)
                false "acceptable-felleisen-answer? test 4 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS :

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-dups : ListOfSlip -> ListOfSlip
;; GIVEN: Input list i.e a ListOfSlip
;; RETURNS: ListOfSlip without any duplicates in the list.
;; EXAMPLES:
;; (remove-dups INCORRECT-FELL-LIST) = CORRECT-FELL-LIST

;; STRATEGY: Use HOF foldr and lambda on lst

(define (remove-dups lst)
  (foldr (lambda (x y)
           (cons x (filter (lambda (z) (not (slips-equal? x z))) y)))
         empty lst))

;; TESTS :

(begin-for-test
  (check-equal? (remove-dups INCORRECT-FELL-LIST) CORRECT-FELL-LIST
                "remove-dups test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return-list-with-color : ListOfSlip Color -> ListOfSlip
;; GIVEN: Input list i.e a ListOfSlip and a Color of Slips.
;; RETURNS: ListOfSlip filtered on basis of given Color.
;; EXAMPLES:
;; (return-list-with-color MIX-LIST COLOR1) = YELLOW-LIST
;; (return-list-with-color MIX-LIST COLOR2) = BLUE-LIST

;; STRATEGY: Use HOF filter and lambda on lst and clr

(define (return-list-with-color lst clr)
  (filter (lambda (x) (equal? (slip-color x) clr)) lst))

;; TESTS :

(begin-for-test
  (check-equal? (return-list-with-color MIX-LIST COLOR1) YELLOW-LIST
                "return list with color test 1 fail")
  (check-equal? (return-list-with-color MIX-LIST COLOR2) BLUE-LIST
                "return list with color test 2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return-member-list : ListOfSlip ListOfSlip -> ListOfSlip
;; GIVEN: Input list and a list to match it with.
;; RETURNS: ListOfSlip having having items of lst1 that
;;          are members of lst2.
;; EXAMPLES:
;; (return-member-list CORRECT-FELL-LIST INCORRECT-FELL-LIST) =
;; CORRECT-FELL-LIST
;; (return-member-list CORRECT-SHIV-LIST INCORRECT-SHIV-LIST) =
;; CORRECT-SHIV-LIST

;; STRATEGY: Use HOF filter and lambda on lst1 and lst2

(define (return-member-list lst1 lst2)
  (filter (lambda (x) (slip-exits? x lst2)) lst1))

;; TESTS :

(begin-for-test
  (check-equal? (return-member-list CORRECT-FELL-LIST INCORRECT-FELL-LIST)
                CORRECT-FELL-LIST "return member list test1 fail")
  (check-equal? (return-member-list CORRECT-SHIV-LIST INCORRECT-SHIV-LIST)
                CORRECT-SHIV-LIST "return member list test2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rev-name-slip : Slip -> Slip
;; GIVEN: A Slip
;; RETURNS: A Slip with first and last name reversed

;; EXAMPLES:
;; (rev-name-slip n1) = n3

;; STRATEGY: using template of Slip

(define (rev-name-slip s)
 (make-slip (slip-color s) (slip-name2 s)
                     (slip-name1 s)))

;; TESTS :

(begin-for-test
  (check-equal? (rev-name-slip n1) n3 "rev-name test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slip-exits? : Slip List -> Boolean
;; GIVEN: A Slip and a List
;; RETURNS: if the slip exits in the list

;; EXAMPLES:
;; (slip-exists? n1 CORRECT-FELL-LIST) = true
;; (slip-exists? n6 CORRECT-FELL-LIST) = false

;; STRATEGY: combining simpler functions

(define (slip-exits? s l)
  (or
   (member s l)
   (member (rev-name-slip s) l)
   ))

;; TESTS :

(begin-for-test
  (check-equal? (slip-exits? n1 CORRECT-FELL-LIST) true
                "slip-exits? test 1 fail")
  (check-equal? (slip-exits? n14 CORRECT-FELL-LIST) false
                "slip-exits? test 2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slips-equal? : Slip Slip -> Boolean
;; GIVEN: Two Slips
;; RETURNS: true if both slips are equal or false otherwise

;; EXAMPLES:
;; (slips-equal? n1 n3) = true
;; (slips-equal? n1 n2) = false

;; STRATEGY: combining simpler functions

(define (slips-equal? s1 s2)
  (or
   (equal? s1 s2)
   (equal? (rev-name-slip s1) s2)
   ))

;; TESTS :

(begin-for-test
  (check-equal? (slips-equal? n1 n3) true
                "slips-equal test 1 fail")
  (check-equal? (slips-equal? n1 n2) false
                "slips-equal test 1 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

