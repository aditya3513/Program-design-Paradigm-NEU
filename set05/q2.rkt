;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "05" "q2.rkt")

(provide make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         roster-classname
         roster-students
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-1
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOR TESTING PURPOSE DATA DECLARATION :

;;;;; FOR STUDENT : 
;; CASE 1 : CONSIDERING STUDENT AS A STRING

;; An Student is a String which represents name of the student.
(define STUDENT-STR1 "John")
(define STUDENT-STR2 "Kathryn")
(define STUDENT-STR3 "Feng")
(define STUDENT-STR4 "Amy")

;; CASE 2 : CONSIDERING STUDENT AS A STRUCT WITH NAME AS ITS ONLY PARAMETER

(define-struct student (name))

;; An Student is a (make-student String).
;; Interpretation: 
;; name represents the name of the student

;; template:
;; student-fn : Student -> ??
;(define (student-fn s)
;  (... (student-name s)))

(define STUDENT-STRCT1 (make-student "John"))
(define STUDENT-STRCT2 (make-student "Kathryn"))
(define STUDENT-STRCT3 (make-student "Feng"))
(define STUDENT-STRCT4 (make-student "Amy"))

;;;;; FOR CLASS :
;; CASE 1 : CONSIDERING CLASS AS A STRING

;; An Class is a String which represents name of the class.
(define CLASS-STR1 "pdp")
(define CLASS-STR2 "networks")

(define dummy-list (list CLASS-STR1 CLASS-STR2 CLASS-STR1 CLASS-STR1
                         CLASS-STR2))

;; CASE 2 : CONSIDERING CLASS AS A STRUCT WITH CLASS NAME A ONLY PARAMATER

(define-struct class (name))

;; An Class is a (make-class String).
;; Interpretation: 
;; name represents the name of the class

;; template:
;; class-fn : Class -> ??
;(define (class-fn c)
;  (... (class-name c)))

(define CLASS-STRCT1 (make-class "pdp"))
(define CLASS-STRCT2 (make-class "networks"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; A Student can be any data type (unspecified)
;; Student represents student's name

;; A Class can be any data type (unspecified)
;; Class represnts the class in which a student is enrolled

;-----------

(define-struct enrollment (student class))

;; An EnrollmentAssertion is a (make-enrollment Student Class).
;; Interpretation: 
;; A (make-enrollment s c) represents the assertion that 
;; student s is enrolled in class c.

;; template:
;; enrollment-fn : EnrollmentAssertion -> ??
;(define (enrollment-fn e)
;  (... (enrollment-student e)
;       (enrollment-class e)))

;; EXAMPLES :

;;;; String enrollments
(define ENROLLMENT-STR1 (make-enrollment STUDENT-STR1 CLASS-STR1))
(define ENROLLMENT-STR2 (make-enrollment STUDENT-STR2 CLASS-STR2))
(define ENROLLMENT-STR3 (make-enrollment STUDENT-STR3 CLASS-STR1))
(define ENROLLMENT-STR4 (make-enrollment STUDENT-STR4 CLASS-STR1))
(define ENROLLMENT-STR5 (make-enrollment STUDENT-STR4 CLASS-STR2))

(define SET-ENROLLMENT-STR (list ENROLLMENT-STR1 ENROLLMENT-STR2
                                 ENROLLMENT-STR3 ENROLLMENT-STR4
                                 ENROLLMENT-STR5))

(define SET-ENROLLMENT-STR2 (list ENROLLMENT-STR1 ENROLLMENT-STR2
                                 ENROLLMENT-STR3 ENROLLMENT-STR4))

(define SET-ENROLLMENT-STR3 (list ENROLLMENT-STR4 ENROLLMENT-STR2
                                 ENROLLMENT-STR5 ENROLLMENT-STR1
                                 ENROLLMENT-STR3))

;;;; Sruct enrollments

(define ENROLLMENT-STRCT1 (make-enrollment STUDENT-STRCT1 CLASS-STRCT1))
(define ENROLLMENT-STRCT2 (make-enrollment STUDENT-STRCT2 CLASS-STRCT2))
(define ENROLLMENT-STRCT3 (make-enrollment STUDENT-STRCT3 CLASS-STRCT1))
(define ENROLLMENT-STRCT4 (make-enrollment STUDENT-STRCT4 CLASS-STRCT1))
(define ENROLLMENT-STRCT5 (make-enrollment STUDENT-STRCT4 CLASS-STRCT2))

(define SET-ENROLLMENT-STRCT (list ENROLLMENT-STRCT1 ENROLLMENT-STRCT2
                                   ENROLLMENT-STRCT3 ENROLLMENT-STRCT4
                                   ENROLLMENT-STRCT5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct roster (class students))

;; A ClassRosterAssertion is a (make-roster Class SetOfStudent).
;; Interpretation: 
;; A (make-roster c ss)represents the assertion that 
;; the students in class c are exactly the students in set ss.

#|
A SetOfX is a list of X's without duplication.  Two SetOfX's are
considered equal if they have the same members.
|#

;; template:
;; roster-fn : ClassRosterAssertion -> ??
;(define (roster-fn r)
;  (... (roster-class r)
;       (roster-students r)))

;; EXAMPLES :

;; STRING STUDENT NAME LIST
(define ROSTER-STR-LST1 (list STUDENT-STR1 STUDENT-STR3 STUDENT-STR4))
(define ROSTER-STR-LST2 (list STUDENT-STR2 STUDENT-STR4))
(define ROSTER-STR-LST3 (list STUDENT-STR1 STUDENT-STR3 STUDENT-STR4 STUDENT-STR4))
(define ROSTER-STR-LST4 (list STUDENT-STR2 STUDENT-STR4 STUDENT-STR4))



;; STRUCT STUDENT NAME LIST
(define ROSTER-STRCT-LST1 (list STUDENT-STRCT1 STUDENT-STRCT3 STUDENT-STRCT4))
(define ROSTER-STRCT-LST2 (list STUDENT-STRCT2 STUDENT-STRCT4))

;;;; STRING ROSTER
(define ROSTER-STR1
  (make-roster CLASS-STR1 ROSTER-STR-LST1))
(define ROSTER-STR2 (make-roster CLASS-STR2 ROSTER-STR-LST2))

(define ROSTER-STR3 (make-roster CLASS-STR2 ROSTER-STR-LST1))
(define ROSTER-STR4 (make-roster CLASS-STR1 ROSTER-STR-LST2))

(define ROSTER-STR5 (make-roster ROSTER-STR-LST1 CLASS-STR1))
(define ROSTER-STR6 (make-roster ROSTER-STR-LST2 CLASS-STR2))

(define ROSTER-STR7 (make-roster CLASS-STR1 ROSTER-STR-LST3))
(define ROSTER-STR8 (make-roster CLASS-STR2 ROSTER-STR-LST4))

(define SET-ROSTER-STR (list ROSTER-STR1 ROSTER-STR2))
(define SET-ROSTER-STR1 (list ROSTER-STR2 ROSTER-STR1))

(define SET-ROSTER-STR2 (list ROSTER-STR4 ROSTER-STR3))

(define SET-ROSTER-STR3 (list ROSTER-STR5 ROSTER-STR6))

(define SET-ROSTER-STR4 (list ROSTER-STR7 ROSTER-STR8))


;;;; STRUCT ROSTER

(define ROSTER-STRCT1
  (make-roster CLASS-STRCT1 ROSTER-STRCT-LST1))

(define ROSTER-STRCT2 (make-roster CLASS-STRCT2 ROSTER-STRCT-LST2))

(define SET-ROSTER-STRCT (list ROSTER-STRCT1 ROSTER-STRCT2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS :

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-dups : List -> List
;; GIVEN: A list which might have duplicate entries.
;; RETURNS: List without any duplicate entries.
;; EXAMPLES:
;; (remove-dups dummy-list) = (list "pdp" "networks")

;; STRATEGY: Use HOF filter, foldr and lambda on lst

(define (remove-dups lst)
  (foldr (lambda (x y)
           (cons x (filter (lambda (z) (not (equal? x z))) y)))
         empty lst))

;; TESTS :

(begin-for-test
  (check-equal? (remove-dups dummy-list) (list CLASS-STR1 CLASS-STR2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return-list-by-classes : SetOfEnrollmentAssertion -> ListofClasses
;; GIVEN: A SetOfEnrollmentAssertion.
;; RETURNS: List of classes that are in SetOfEnrollmentAssertion.
;; EXAMPLES:
;; (return-list-by-classes SET-ENROLLMENT-STR) = (list "pdp" "networks")

;; STRATEGY: Use HOF map on lst.

(define (return-list-by-classes lst)
  (remove-dups(map (lambda (x) (enrollment-class x)) lst)))

;; TESTS :

(begin-for-test
  (check-equal? (return-list-by-classes SET-ENROLLMENT-STR)
                (list "pdp" "networks")
                "return-list-by-classes test fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; roster-classname : ClassRosterAssertion -> Class
;; GIVEN: a ClassRosterAssertion r
;; RETURNS: A class in which students are enrolled

;; STRATEGY : using template of ClassRosterAssertion on r
;; EXAMPLES :
;; (roster-classname ROSTER-STR1) = CLASS-STR1
;; (roster-classname ROSTER-STRCT2) = CLASS-STRCT2

(define (roster-classname r)
  (roster-class r))

;; TESTS :

(begin-for-test
  (check-equal? (roster-classname ROSTER-STR1) CLASS-STR1
                "roster-classname test1 fail")
  (check-equal? (roster-classname ROSTER-STRCT2) CLASS-STRCT2
                "roster-classname test2 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-list-equal? : ListofStudents ListofStudents -> Boolean
;; GIVEN: two ListofStudents l1 and l2 to compare.
;; RETURNS: true if lists are equal otherwise false
;; EXAMPLES :
;; (check-list-equal? ROSTER-STR-LST1 ROSTER-STR-LST1) = true
;; (check-list-equal? ROSTER-STR-LST1 ROSTER-STR-LST3) = false

;; STRATEGY : using HOF's andmap, lambda on l1 and l2


(define (check-list-equal? l1 l2)
  (and
   (and
   (andmap (lambda (x) (member x l1)) l2)
   (andmap (lambda (x) (member x l2)) l1))
   (= (length l1) (length l2))))

;; TESTS :

(begin-for-test
  (check-equal? (check-list-equal? ROSTER-STR-LST1 ROSTER-STR-LST1)
                true "check-list-equal test 1 fail")
  (check-equal? (check-list-equal? ROSTER-STR-LST1 ROSTER-STR-LST3)
                false "check-list-equal test 1 fail"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compare-complex-list? : SetOfClassRosterAssertion SetOfClassRosterAssertion -> Boolean
;; GIVEN: two SetOfClassRosterAssertion l1 and l2 to compare.
;; RETURNS: true if lists are equal otherwise false
;; EXAMPLES :
;; (compare-complex-list? SET-ROSTER-STR SET-ROSTER-STR) = true
;; (compare-complex-list? SET-ROSTER-STR SET-ROSTER-STR) = false

;; STRATEGY : using HOF's andmap,ormap and lambda on l1 and l2

(define (compare-complex-list? lst1 lst2)
  (andmap (lambda (x)
          (ormap (lambda (y)
                   (and (equal? (roster-classname x) (roster-classname y))
                        (check-list-equal? (roster-students x) (roster-students y))))
                 lst1)) lst2))

;; TESTS :

;; see test cases of behaviour-correct?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; enrollments-to-rosters : SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollments
;; RETURNS: a correct set of class rosters for the given enrollments
;; EXAMPLES :
;; (enrollments-to-rosters SET-ENROLLMENT-STR) = SET-ROSTER-STR
;; (enrollments-to-rosters SET-ENROLLMENT-STRCT) = SET-ROSTER-STRCT

;; STRATEGY : using HOF's map, lambda and filter on e

(define (enrollments-to-rosters e)
  (map (lambda (x)
         (make-roster x
                      (map (lambda (z) (enrollment-student z))
                           (filter (lambda (y) (equal? (enrollment-class y) x)) e)) ))
       (return-list-by-classes e)))

;; TESTS :

(begin-for-test
  (check-equal? (enrollments-to-rosters SET-ENROLLMENT-STR) SET-ROSTER-STR
                "enrollments-to-rosters test1 fail")
  (check-equal? (enrollments-to-rosters SET-ENROLLMENT-STRCT) SET-ROSTER-STRCT
                "enrollments-to-rosters test2 fail"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; enrollments-to-rosters-bad-1 : SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;; Here the classes are switched to opposite values
;; rosters.

;; EXAMPLES :
;; (enrollments-to-rosters-bad-1 SET-ENROLLMENT-STR) = SET-ROSTER-STR2
;; STRATEGY : using HOF lambda, map and filter on e

(define (enrollments-to-rosters-bad-1 e)
  (map (lambda (x)
         (make-roster x
                      (map (lambda (z) (enrollment-student z))
                           (filter (lambda (y) (not (equal? (enrollment-class y) x))) e)) ))
       (return-list-by-classes e)))

;; TESTS :

(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-1 SET-ENROLLMENT-STR)
                SET-ROSTER-STR2 "bad roster 1 test fail"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;; it swaps student list with class name
;; rosters.

;; EXAMPLES :
;; (enrollments-to-rosters-bad-2 SET-ENROLLMENT-STR) = SET-ROSTER-STR3
;; STRATEGY : using HOF lambda, map and filter on e

(define (enrollments-to-rosters-bad-2 e)
  (map (lambda (x)
         (make-roster 
                      (map (lambda (z) (enrollment-student z))
                           (filter (lambda (y) (equal? (enrollment-class y) x)) e)) x))
       (return-list-by-classes e)))

;; TESTS :

(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-2 SET-ENROLLMENT-STR)
                SET-ROSTER-STR3 "bad roster 2 test fail"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;; this return the roster with duplicate values.

;; EXAMPLES :
;; (enrollments-to-rosters-bad-3 SET-ENROLLMENT-STR) = SET-ROSTER-STR4

;; STRATEGY : using HOF lambda, map and filter on e

(define (enrollments-to-rosters-bad-3 e)
  (map (lambda (x)
         (make-roster x
                      (map (lambda (z) (enrollment-student z))
                           (filter (lambda (y)
                                     (or (equal? (enrollment-class y) x)
                                         (equal? (enrollment-student y) "Amy"))) e))))
       (return-list-by-classes e)))


;; TESTS :
(begin-for-test
  (check-equal?(enrollments-to-rosters-bad-3 SET-ENROLLMENT-STR)
               SET-ROSTER-STR4 "bad roster 3 test fail"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;; GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;; RETURNS: true iff the output of soln-fn on se is an example of correct
;; behavior by a ProposedSolution.

;; EXAMPLES:
;; (behavior-correct? enrollments-to-rosters SET-ENROLLMENT-STR) = true
;; (behavior-correct? enrollments-to-rosters-bad-1 SET-ENROLLMENT-STR) = false
;; (behavior-correct? enrollments-to-rosters-bad-2 SET-ENROLLMENT-STR) = false
;; (behavior-correct? enrollments-to-rosters-bad-3 SET-ENROLLMENT-STR) = false

;; STRATEGY : combining simpler functions


(define (behavior-correct? soln-fn se)
  (compare-complex-list? (soln-fn se) (enrollments-to-rosters se)))

;; TESTS :

(begin-for-test
  (check-equal? (behavior-correct? enrollments-to-rosters SET-ENROLLMENT-STR3)
                true "behaviour correct test 1 fail")
(check-equal? (behavior-correct? enrollments-to-rosters-bad-1 SET-ENROLLMENT-STR)
              false "behaviour correct test 2 fail")
(check-equal? (behavior-correct? enrollments-to-rosters-bad-2 SET-ENROLLMENT-STR)
              false "behaviour correct test 3 fail")
(check-equal? (behavior-correct? enrollments-to-rosters-bad-3 SET-ENROLLMENT-STR)
              false "behaviour correct test 4 fail"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

