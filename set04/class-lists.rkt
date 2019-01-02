;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "04" "class-lists.rkt")


(provide make-slip
         slip-color
         slip-name1
         slip-name2
         felleisen-roster
         shivers-roster
         possible-roster?
         acceptable-felleisen-answer?)

;;; DATA DEFINITIONS
(define-struct slip (color name1 name2))
;; A Slip is a (make-slip String String String)
;; Interpretation: 
;; color is blue or yellow.
;; name1 is the first name or last name of student.
;; name2 is the last name or first name of student.

;; template:
;; slip-fn : Slip -> ??
;(define (slip-fn s)
;  (... (slip-color s)
;       (slip-name1 s)
;       (slip-name2 s)))

;;examples of slip, for testing
(define slip1 (make-slip "yellow" "abc" "def"))  
(define slip2 (make-slip "blue" "xyz" "pqr"))


;; Definations for test cases
(define n1 (make-slip "yellow" "a" "b"))
(define n2 (make-slip "yellow" "x" "y"))
(define n3 (make-slip "yellow" "b" "a"))
(define n4 (make-slip "yellow" "ab" "bz"))
(define n5 (make-slip "yellow" "abc" "b"))
(define n6 (make-slip "yellow" "a" "b"))
(define n7 (make-slip "yellow" "a1" "b1"))

(define n11 (make-slip "yellow" "a" "b"))
(define n12 (make-slip "yellow" "x" "y"))
(define n13 (make-slip "yellow" "b" "a"))
(define n14 (make-slip "blue" "ab" "bz"))
(define n15 (make-slip "yellow" "abc" "b"))
(define n16 (make-slip "yellow" "a" "b"))
(define n17 (make-slip "blue" "a1" "b1"))
(define n18 (make-slip "blue" "a1" "b1"))
(define n19 (make-slip "blue" "ab" "b1"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Shivers' class, without duplication.

(define (shivers-roster l)
  (remove-dups (shivers-helper l empty) empty))

;; (shivers-roster (list n11 n12 n13 n14 n15 n16 n17 n18 n19))

;; TESTS :

(begin-for-test
  (check-equal? 
    (shivers-roster (list n11 n12 n13 n14 n15 n16 n17 n18 n19))
                    (list n14 n17 n19)
    "it should return given list")
  (check-equal? 
    (shivers-roster (list n14 n17 n19))
                    (list n14 n17 n19)
    "it should return given list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Felleisen's class, without duplication.

(define (felleisen-roster l)
  (remove-dups (felleisen-helper l empty) empty))

;; TESTS :

;; TESTS :

(begin-for-test
  (check-equal? 
    (felleisen-roster (list n11 n12 n13 n14 n15 n16 n17))
                    (list n2 n5 n6)
    "it should return given list")
  (check-equal? 
    (felleisen-roster (list n2 n5 n6))
                      (list n2 n5 n6)
    "it should return given list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
                  
(define (possible-roster? l)
  (or
   (equal? (shivers-roster l) l)
   (equal? (felleisen-roster l) l)))

;; TESTS :

(begin-for-test
  (check-equal? 
    (possible-roster? (list n11 n12 n13 n14 n15 n16 n17)) false
    "It is not a roster")
  (check-equal? 
    (possible-roster? (list  n14 n19 n17)) true
    "It is a shiver roster")
  (check-equal? 
    (possible-roster? (list n11 n12 n15)) true
    "It is fellesian a roster"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS :

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color-yellow? : Slip -> Boolean
;; GIVEN: A Slip
;; RETURNS: if the color is yellow or not

;; EXAMPLES: See test cases

;; STRATEGY: using template of Slip

(define (color-yellow? s)
  (equal? (slip-color s) "yellow"))

;; tests:
(begin-for-test
  (check-equal? 
    (color-yellow? slip1) true
    "It should return true as color is yellow")
  (check-equal? 
    (color-yellow? slip2) false
    "It should return false as color is not yellow"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color-yellow? : Slip -> Boolean
;; GIVEN: A Slip
;; RETURNS: if the color is blue or not

;; EXAMPLES: See test cases

;; STRATEGY: using template of Slip

(define (color-blue? s)
  (equal? (slip-color s) "blue"))

;; tests:
(begin-for-test
  (check-equal? 
    (color-blue? slip2) true
    "It should return true as color is blue")
  (check-equal? 
    (color-blue? slip1) false
    "It should return false as color is not blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rev-name-slip : Slip -> Slip
;; GIVEN: A Slip
;; RETURNS: A lip with first and last name reversed

;; EXAMPLES: See test cases

;; STRATEGY: using template of Slip

(define (rev-name-slip s)
 (make-slip (slip-color s) (slip-name2 s)
                     (slip-name1 s)))
;; tests:
(begin-for-test
  (check-equal? 
    (rev-name-slip slip2)
    (make-slip (slip-color slip2) (slip-name2 slip2) (slip-name1 slip2))
    "It should return slip with reversed names")
  (check-equal? 
    (rev-name-slip slip1)
    (make-slip (slip-color slip1) (slip-name2 slip1) (slip-name1 slip1))
    "It should return slip with reversed names"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slip-exits? : Slip List -> Boolean
;; GIVEN: A Slip and a List
;; RETURNS: if the slip exits in the list

;; EXAMPLES: See test cases

;; STRATEGY: combining simpler functions

(define (slip-exits? s l)
  (or
   (member s l)
   (member (rev-name-slip s) l)
   ))

;; tests:
(define lst1 (list slip1 slip2))
(begin-for-test
  (check-equal? 
    (slip-exits? slip2 lst1)
    true
    "It should return true as the slip is in the list")
  (check-equal? 
    (slip-exits? (make-slip "yellow" "a" "b") lst1)
    false
    "It should return false as the slip is not in the list"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-dups : List List -> List
;; GIVEN: Input list and an empty list
;; RETURNS: List without any duplicates in the list

;; EXAMPLES: See test cases

;; STRATEGY: combining simpler functions


(define (remove-dups l1 l2)
  (cond
    [(empty? l1) empty]
    [else
     (if (not (slip-exits? (first l1) (rest l1)))
         (append (cons (first l1) l2) (remove-dups (rest l1) l2))
         (remove-dups (rest l1) l2)
         )]))

;; TESTS :
(define lst2 (list n2 n4 n5 n6 n7))

(begin-for-test
  (check-equal? 
   (remove-dups (list n1 n2 n3 n3 n4 n4 n5 n6 n7) empty) lst2
    "It should return the second list")
  (check-equal? 
   (remove-dups lst2 empty) lst2
    "It should return the same list as no duplicates"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-helper : List List -> List
;; GIVEN: Input list and an empty list
;; RETURNS: List having only yellow color

;; EXAMPLES: See test cases

;; STRATEGY: combining simpler functions



(define (felleisen-helper l1 l2)
  (cond
    [(empty? l1) empty]
    [else
     (if (color-yellow? (first l1))
         (append (cons (first l1) l2) (felleisen-helper (rest l1) l2))
         (felleisen-helper (rest l1) l2)
         )]))

;; TESTS :
(define lst3 (list n11 n12 n13 n14 n15 n16 n17))

(begin-for-test
  (check-equal? 
   (felleisen-helper lst3 empty) (list n11 n12 n13 n15 n16)
    "It should return the second list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-helper : List List -> List
;; GIVEN: Input list and an empty list
;; RETURNS: List having only blue color

;; EXAMPLES: See test cases

;; STRATEGY: combining simpler functions


(define (shivers-helper l1 l2)
  (cond
    [(empty? l1) empty]
    [else
     (if (color-blue? (first l1))
         (append (cons (first l1) l2) (shivers-helper (rest l1) l2))
         (shivers-helper (rest l1) l2)
         )]))

;; TESTS :
(define lst4 (list n11 n12 n13 n14 n15 n16 n17 n18 n19))

(begin-for-test
  (check-equal? 
   (shivers-helper lst4 empty) (list n14 n17 n18 n19)
    "It should return the second list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; acceptable-felleisen-helper? : ListOfSlip ListOfSlip ListOfSlip -> ListOfSlip
;; GIVEN: two lists of slips, lst1 and lst2 and an empty list
;; RETURNS: a list of element of list1 that exist in list2.

(define (acceptable-felleisen-helper l1 l2 l3)
  (cond
    [(empty? l1) empty]
    [else
     (if (slip-exits? (first l1) l2)
         (append (cons (first l1) l3) (acceptable-felleisen-helper (rest l1) l2 l3))
         (acceptable-felleisen-helper (rest l1) l2 l3)
         )]))

;; TESTS :
(begin-for-test
  (check-equal? 
   (acceptable-felleisen-helper (list n1 n2 n3 n14 n5) (list n1 n2 n5) empty)
   (list n1 n2 n3 n5)
    "It should return the second list"))


;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;;          and only once in lst2.


(define (acceptable-felleisen-answer? l1 l2)
  (equal? (length (felleisen-roster l1))
          (length (acceptable-felleisen-helper (felleisen-roster l1)
                                               (felleisen-roster l2)
                                               empty))))

;; TESTS :
(begin-for-test
  (check-equal? 
   (acceptable-felleisen-answer? (list n1 n2 n3 n14 n5) (list n1 n2 n5))
   true
    "It should return true")
  (check-equal? 
   (acceptable-felleisen-answer? (list n1 n7 n5) (list n1 n2 n5))
   false
    "It should return false"))


