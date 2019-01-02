;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 probe-possible-outcome?
 make-turn-right
 make-turn-left
 make-move-forward)

(define NORTH "north")
(define EAST "east")
(define WEST "west")
(define SOUTH "south")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definations

;; A Direction is either :
;; - EAST
;; - WEST
;; - NORTH
;; - SOUTH

;; TEMPLATE :
;; direction-fn : Direction -> ??
;; (define (direction-fn d)
;;    (...
;;        (string=? d WEST)
;;        (string=? d EAST)
;;        (string=? d NORTH)
;;        (string=? d SOUTH)))

;;--------------------------------------------------------------------;;

(define-struct turn-right ())
;; A Turn-Right is a (make-turn-right)
;; Interpretation:
;; A struct without any field, will be used for making the bot turn right.

;; TEMPLATE:
;; turn-right-fn : Turn-Right -> ??
;(define (turn-right-fn)
;  (...))

(define RIGHT (make-turn-right))
;;--------------------------------------------------------------------;;

(define-struct turn-left ())
;; A Turn-Left is a (make-turn-left)
;; Interpretation:
;; A struct without any field, will be used for making the bot turn left.

;; TEMPLATE:
;; turn-left-fn : Turn-Left -> ??
;(define (turn-left-fn)
;  (...))

(define LEFT (make-turn-left))
;;--------------------------------------------------------------------;;

(define-struct move-forward (steps))
;; A Move-Forward is a (make-move-forward PosInt)
;; Interpretation:
;; A struct with a field steps and is used for making bot move forward.
;; steps is the number of steps to move forward.

;; TEMPLATE:
;; move-forward-fn : Move-Forward -> ??
;(define (move-forward-fn s)
;  (...(move-forward-steps s)))

(define FWD-10 (make-move-forward 10))
(define FWD-5 (make-move-forward 5))


;;--------------------------------------------------------------------;;

;; A ListOfInstruction (LOI) is either :
;; - empty
;; - (cons Instruction ListOfInstruction)

;; template :
;; listofinstruction-fn : LOI -> ??
;(define (listofinstruction-fn loi)
; (cond
;   [(empty? loi) ...]
;   [(else ...
;         (instruction-fn (first loi)
;         (listofinstruction-fn (rest loi))))]))

(define LOI1 (list RIGHT FWD-10 RIGHT FWD-5))
;;--------------------------------------------------------------------;;



;; A Program is a ListOfInstruction
;; Interp: A sequence of instructions, to be executed from left to
;; right. 

;; An Instruction is one of
;; -- (make-turn-left)            Interp: a turn-left instruction
;; -- (make-turn-right)           Interp: a turn-right instruction
;; -- (make-move-forward PosInt)  Interp: an instruction to move forward
;;                                       the given number of steps.

(define PRG1 LOI1)
(define PRG2 empty)
(define PRG3 (list (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-left)
                    (make-turn-left)))

;;--------------------------------------------------------------------;;  

(define-struct possible-position (max-x min-x max-y min-y))
;; A Possible-Position is a (make-possible-position Int Int Int Int)
;; Interpretation:
;; max-x is the max x-coordinate value of probe after instructions
;; min-x is the min x-coordinate value of probe after instructions
;; max-y is the max y-coordinate value of probe after instructions
;; min-y is the min y-coordinate value of probe after instructions

;; template:
;; possible-position-fn : Possible-Position -> ??
;(define (possible-position-fn pp)
; (...
;  (possible-position-max-x pp)
;  (possible-position-min-x pp)
;  (possible-position-max-y pp)
;  (possible-position-min-y pp)))

(define PP1 (make-possible-position 32 28 107 103))


;;--------------------------------------------------------------------;;
;;; scalable-stress-program : Int -> Program
;;; Given: an integer n
;;; Returns: a Program of length (* 6 (max 0 n))
;; DESIGN STRATEGY : using templates of turn-right, turn-left and
;;                   move-forward
;; HALTING MEASURE : value of n

(define (scalable-stress-program n)
  (if (> n 0)
      (append (list (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-left)
                    (make-turn-left))
              (scalable-stress-program (- n 1)))
      empty))

;; TEST :
(begin-for-test
  (check-equal? (scalable-stress-program 1) PRG3
                "scalable-stress-program test fail"))

;;--------------------------------------------------------------------;;

;;; stress-benchmark : Int -> Boolean
;;; Given: an integer, preferably non-negative, stating the desired
;;;     degree of difficulty for the benchmark
;;; Returns: true
;;; Effect: reports how much time probe-possible-outcome?
;;;     takes to compute the correct answer
;; DESIGN STARTEGY : combining simpler function
;; EXAMPLES : See test cases.

(define (stress-benchmark n)
  (let ((pgm (scalable-stress-program n)))
    (time (probe-possible-outcome? 0 0 pgm (* 10 n) (* 10 n)))))

;; TEST :
(begin-for-test
  (check-equal? (stress-benchmark 10) true
                "stress-benchmark test fail"))
  
;;--------------------------------------------------------------------;;
;; probe-turn-left : Direction -> Direction
;; GIVEN: A Direction d of current probe 
;; RETURNS: A new direction after turn left from given direction

;; DESIGN STRATEGY: use template of direction
;; EXAMPLES :
;; (probe-turn-left NORTH) = WEST
;; (probe-turn-left EAST) = NORTH

(define (probe-turn-left d)
  (cond
    [(string=? d NORTH) WEST]
    [(string=? d WEST) SOUTH]
    [(string=? d SOUTH) EAST]
    [else NORTH]))


;; TESTS :
(begin-for-test
  (check-equal? (probe-turn-left NORTH) WEST
                "test 1 for probe-turn-left fail")
  (check-equal? (probe-turn-left EAST) NORTH
                "test 2 for probe-turn-left fail")
  (check-equal? (probe-turn-left WEST) SOUTH
                "test 3 for probe-turn-left fail")
  (check-equal? (probe-turn-left SOUTH) EAST
                "test 4 for probe-turn-left fail"))


;;--------------------------------------------------------------------;;

;; probe-turn-right : Direction -> Direction
;; GIVEN: A Direction d
;; RETURNS: A new direction after turning right from given direction

;; DESIGN STRATEGY: use template of direction
;; EXAMPLES :
;; (probe-turn-right NORTH) = EAST
;; (probe-turn-right WEST) = NORTH

(define (probe-turn-right d)
  (cond
    [(string=? d NORTH) EAST]
    [(string=? d WEST) NORTH]
    [(string=? d SOUTH) WEST]
    [else SOUTH]))


;; TESTS :
(begin-for-test
  (check-equal? (probe-turn-right NORTH) EAST
                "test 1 for probe-turn-right fail")
  (check-equal? (probe-turn-right WEST) NORTH
                "test 2 for probe-turn-right fail")
  (check-equal? (probe-turn-right SOUTH) WEST
                "test 3 for probe-turn-right fail")
  (check-equal? (probe-turn-right EAST) SOUTH
                "test 4 for probe-turn-right fail"))

;;------------------------------------------------------------------------;;
;; update-direction : Direction Insturction -> Direction
;; GIVEN: A Direction d and a insturction
;; WHERE: d is the current direction and instruction is about turning
;; EXAMPLES:
;;  (update-direction NORTH LEFT-INSTURCTION)) = WEST
;; DESIGN STRATEGY: use template of Instruction

(define (update-direction d instruction)
  (cond
    [(equal? instruction LEFT)
     (probe-turn-left d)]
    [else
     (probe-turn-right d)]))

;; TESTS :
(begin-for-test
  (check-equal? (update-direction NORTH LEFT) WEST
                "test 1 for update-direction fail")
  (check-equal? (update-direction NORTH RIGHT) EAST
                "test 2 for update-direction fail"))


;;------------------------------------------------------------------------;;

;; after-instruction : Direction Program Possible-Position -> Possible-position
;; GIVEN: A Direction d, a Program program and a Possible-Position pp
;; RETURN: A new possible-position after running the given program 
;; DESIGN STRATEGY : divide on cases by value of prgram
;; HALTING MEASURE : length of Program i.e LOI
;; EXAMPLES:
;; 

(define (after-instruction d program pp)
  (cond
    [(empty? program) pp]
    [(or (equal? (first program) LEFT)
         (equal? (first program) RIGHT))
     (after-instruction (update-direction d (first program))
                        (rest program)
                        pp)]
    [else
     (after-instruction d (rest program)
                        (update-possible-position (move-forward-steps (first program))
                                                  d
                                                  pp))]))


;; TESTS :
(define PP4 (make-possible-position 44 36 114 106))

(begin-for-test
  (check-equal? (after-instruction NORTH PRG1 PP1) PP4
                "make-possible-position test 1 fail"))


;;------------------------------------------------------------------------;;
;; update-possible-position : PosInt Direction Possible-Position
;;                                             -> Possible-Position
;; GIVEN: A PosInt movement, a Direction direction and a Possible-Position p
;; RETURN: A new Possible-Position after move movement steps in given direction
;; DESIGN STRATEGY: combining simpler function
;; EXAMPLES : 

(define (update-possible-position movement direction p)
  (if (or (string=? direction NORTH)
          (string=? direction SOUTH))
      (update-possible-y movement direction p)
      (update-possible-x movement direction p)))

;; TEST :

(begin-for-test
  (check-equal? (update-possible-position 1 WEST PP1) PP3
                "update-possible-position test X axis fail")
  (check-equal? (update-possible-position 1 NORTH PP1) PP2
                "update-possible-position test Y axis fail"))

;; update-possible-x : PosInt Direction Possible-Position -> Possible-Position
;; GIVEN: A PosInt movement , a Direction direction and a Possible-Position p
;; RETURN: A new possible-position with move forward movement steps in direction
;; EXMAPLES: SEE TEST CASES
;; DESIGN STRATEGY: use template of possible-position
(define (update-possible-x movement direction p)
  (make-possible-position
   (possible-max-x movement (possible-position-max-x p) direction)
   (possible-min-x movement (possible-position-min-x p) direction)
   (possible-position-max-y p)
   (possible-position-min-y p)))

;; TESTS :

(define PP3 (make-possible-position 32 25 107 103))
(begin-for-test
  (check-equal? (update-possible-x 1 WEST PP1) PP3
                "update-possible-x test fail"))


;; update-possible-y : PosInt Direction -> Possible-Position
;; GIVEN: A PosInt movement , a Direction direction and a Possible-Position p
;; RETURN: A new possible-position with move forward movement steps in direction
;; EXMAPLES:
;; DESIGN STRATEGY: use template of possible-position

(define (update-possible-y movement direction p)
  (make-possible-position
   (possible-position-max-x p)
   (possible-position-min-x p)
   (possible-max-y movement (possible-position-max-y p) direction)
   (possible-min-y movement (possible-position-min-y p) direction)))

;; TESTS :

(define PP2 (make-possible-position 32 28 107 100))
(begin-for-test
  (check-equal? (update-possible-y 1 NORTH PP1) PP2
                "update-possible-y test fail"))

;;------------------------------------------------------------------------;;
;; possible-max-x : PosInt Int Direction -> Int
;; GIVEN: A PosInt movement , a Int x and a Direction direction
;; RETURN: The max possible value of x after move movement steps in direction
;; EXMAPLES:
;; DESIGN STRATEGY: use template of direction
(define (possible-max-x movement x direction)
 (cond
    [(and (equal? movement 1)
          (string=? direction WEST))
     x]
    [(and (equal? movement 1)
          (string=? direction EAST))
     (+ x 3)]
    [(string=? direction WEST)
     (- x (- movement 2))]
    [(string=? direction EAST)
     (+ x (+ movement 2))]))

;; TESTS :

(begin-for-test
  (check-equal? (possible-max-x 1 10 WEST) 10
                " possible-max-x test 1 fail")
  (check-equal? (possible-max-x 1 10 EAST) 13
                " possible-max-x test 2 fail")
  (check-equal? (possible-max-x 10 20 WEST) 12
                " possible-max-x test 3 fail"))



;; possible-min-x : PosInt Int Direction -> Int
;; GIVEN: A PosInt movement , a Int x and a Direction direction
;; RETURN: The min possible value of x after move movement steps in direction
;; EXMAPLES:
;; DESIGN STRATEGY: use template of direction
(define (possible-min-x movement x direction)
  (cond
    [(and (equal? movement 1)
          (string=? direction WEST))
     (- x 3)]
    [(and (equal? movement 1)
          (string=? direction EAST))
     x]
    [(string=? direction WEST)
     (- x (+ movement 2))]
    [(string=? direction EAST)
     (+ x (- movement 2))]))

;; TESTS :

(begin-for-test
  (check-equal? (possible-min-x 1 10 WEST) 7
                " possible-min-x test 1 fail")
  (check-equal? (possible-min-x 1 10 EAST) 10
                " possible-min-x test 2 fail")
  (check-equal? (possible-min-x 10 20 WEST) 8
                " possible-min-x test 3 fail"))


;;------------------------------------------------------------------------;;
;; possible-max-y : PosInt Int Direction -> Int
;; GIVEN: A PosInt movement , a Int y and a Direction direction
;; RETURN: The max possible value of y after move movement steps in direction
;; EXMAPLES:
;; DESIGN STRATEGY: use template of direction
(define (possible-max-y movement y direction)
  (cond
    [(and (equal? movement 1)
          (string=? direction NORTH))
     y]
    [(and (equal? movement 1)
          (string=? direction SOUTH))
     (+ y 3)]
    [(string=? direction NORTH)
     (- y (- movement 2))]
    [(string=? direction SOUTH)
     (+ y (+ movement 2))]))


;; TESTS :
(begin-for-test
  (check-equal? (possible-max-y 1 10 NORTH) 10
                " possible-max-y test 1 fail")
  (check-equal? (possible-max-y 1 10 SOUTH) 13
                " possible-max-y test 2 fail")
  (check-equal? (possible-max-y 10 20 NORTH) 12
                " possible-max-y test 3 fail"))

;;------------------------------------------------------------------------;;

;; possible-min-y : PosInt Int Direction -> Int
;; GIVEN: A PosInt movement , a Int y and a Direction direction
;; RETURN: The min possible value of y after move movement steps in direction
;; EXMAPLES: SEE TEST CASES
;; DESIGN STRATEGY: divide by cases by value of movement

(define (possible-min-y movement y direction)
  (cond
    [(and (equal? movement 1)
          (string=? direction NORTH))
     (- y 3)]
    [(and (equal? movement 1)
          (string=? direction SOUTH))
     y]
    [(string=? direction NORTH)
     (- y (+ movement 2))]
    [(string=? direction SOUTH)
     (+ y (- movement 2))]))

;; TEST :

(begin-for-test
  (check-equal? (possible-min-y 1 10 NORTH) 7
                " possible-min-y test 1 fail")
  (check-equal? (possible-min-y 1 10 SOUTH) 10
                " possible-min-y test 2 fail")
  (check-equal? (possible-min-y 10 20 NORTH) 8
                " possible-min-y test 3 fail"))



;;------------------------------------------------------------------------;;
;; inside-range : Int Int Int -> Boolean
;; GIVEN: A Int x and Int a and b
;; RETURN: true if a<=x<=b
;; EXMAPLE:
;;  (inside-range 4 2 6) = #t
;; DESIGN STRATEGY: combine simpler functions
(define (inside-range x a b)
  (and (>= x a)
       (<= x b)))

;; TEST :

(begin-for-test
  (check-equal? (inside-range 4 2 6) #t
                " inside range test fail"))

;;------------------------------------------------------------------------;;
;; probe-possible-outcome? : Int Int Program Int Int -> Bool
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome? 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].

(define (probe-possible-outcome? x0 y0 prg x1 y1)
  (local((define pp (after-instruction NORTH prg (make-possible-position x0 x0 y0 y0))))
    (and (inside-range x1 (possible-position-min-x pp) (possible-position-max-x pp))
         (inside-range y1 (possible-position-min-y pp) (possible-position-max-y pp)))))

;; TEST :
(begin-for-test
  (check-equal? (probe-possible-outcome? 20 100 PRG1 30 105) true
                "possible out come test 1 fail")
  (check-equal? (probe-possible-outcome? 20 100 PRG1 50 105) false
                "possible out come test 2 fail")
  (check-equal? (probe-possible-outcome? 20 100 PRG2 50 105) false
                "possible out come test 3 fail"))
