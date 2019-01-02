;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; screensaver : Integer -> World
;; GIVEN: the speed of stimulation
;; EFFECT: runs the simulation, starting with given speed
;; RETURNS: the final state of the world
;; EXAMPLES : (screensaver 1)
;; No test cases have been added as the final state depends
;;     on when the stimulation ends.


(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy
         world-after-mouse-event
         circ-after-mouse-event
         circ-selected?
         world-circles
         circle-after-key-event
         circle-pen-down?
         )
(check-location "04" "screensaver-3.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (screensaver speed)
  (big-bang (make-world empty true initial-mse )
            (on-tick world-after-tick speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; CONSTANTS
(define INITIAL-X (/ CANVAS-WIDTH 2))
(define INITIAL-Y (/ CANVAS-HEIGHT 2))
(define INITIAL-VX 0)
(define INITIAL-VY 0)


;; NAVIGATION CONSTANTS
(define X-LEFT 40)
(define X-RIGHT 360)
(define Y-UP 40)
(define Y-DOWN 260)

(define RADIUS 40)
(define RED-DOT (circle 5 "solid" "red"))
(define PEN-DOT (circle 1 "solid" "blue"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct circ (x y vx vy selected? pen? drag points))
;; A Circ is a (make-circ NonNegInt NonNegInt Int Int Boolean Drag Boolean)
;; Interpretation: 
;; x is the x co ordinate of the center of the circle.
;; y is the y co ordinate of the center of the circle.
;; vx is the velocity of the center of circle in x direction(a vector).
;; vy is the velocity of the center of circle in y direction(a vector).
;; selected? tells us if circle is selected
;; pen? tells us if pen is down (true) or up (false)
;; drag keeps track of last x y and vx vy
;; points is the set of points visited by center of circle when pen down until erased.

;; template:
;; circ-fn : Circ -> ??
;(define (circ-fn c)
;  (... (circ-x c) (circ-y c) (circ-vx c)
;       (circ-vy c) (circ-selected? c)
;;      (circ-pen? c) (circ-drag c) (circ-points c)))


(define-struct drag (x y vx vy))
;; A Drag is a (make-point Int Int Int Int)
;; Interpretation: 
;; x is x co ordinate.
;; y is y co ordinate
;; vx is speed of x co ordinate.
;; vy is speed of y co ordinate.

;; template:
;; drag-fn : Drag -> ??
;(define (drag-fn d)
;  (... (drag-x d) (drag-y d)
;;      (drag-vx d) (drag-vy d)))

;;examples of circles, for testing
(define initial-drag (make-drag 0 0 0 0))
(define circle1 (make-circ 200 100 -12 20 false false initial-drag empty))  
(define circle2 (make-circ 200 100 -12 20 true false initial-drag empty))

(define circle1-pen-active (make-circ 200 100 -12 20 false true initial-drag empty)) 

(define C1 (make-circ 370 100 20 -10 false false initial-drag empty))
;; moves out of right boundry
(define C2 (make-circ 30 100 -20 -10 false false initial-drag empty))
;; moves out of left boundry
(define C3 (make-circ 200 30 20 -10 false false initial-drag empty))
;; moves out of upper boundry
(define C4 (make-circ 200 280 20 10 false false initial-drag empty))
;; moves out of lower boundry


(define C1-pen-active (make-circ 370 100 20 -10 false true initial-drag empty))
;; moves out of right boundry
(define C2-pen-active (make-circ 30 100 -20 -10 false true initial-drag empty))
;; moves out of left boundry
(define C3-pen-active (make-circ 200 30 20 -10 false true initial-drag empty))
;; moves out of upper boundry
(define C4-pen-active (make-circ 200 280 20 10 false true initial-drag empty))


(define-struct mse (mx my pressed?))
;; A Mse is a (make-mse Int Int Boolean)
;; Interpretation: 
;; x is x co ordinate of mouse click.
;; y is y co ordinate of mouse click
;; pressed? yells if mouse is pressed or not.

;; template:
;; mse-fn : Mse -> ??
;(define (mse-fn m)
;  (... (mse-x m) (mse-y m) (mse-pressed? m)))

(define initial-mse (make-mse 0 0 false))


(define-struct world (circles paused? mse_pt))
;; A WorldState or World is a (make-world ListOfCircle Boolean)
;; Interpretation: 
;; circles is the ListOfCircle.
;; paused? describes whether or not the screensaver is paused.
;;   describes the   of stimulation in seconds/tick.
;; mse_pt is for holdinf most recent co ordinates of mouse click.


;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-circles w) (world-paused? w)
;;      (world-mse_pt w) ))


;;examples of worldstate, for testing
(define list_of_circles (list circle1 circle2))
(define unpaused-world (make-world list_of_circles false initial-mse ))  
(define paused-world   (make-world list_of_circles true initial-mse ))



(define-struct point (x y))
;; A Point is a (make-point Int Int)
;; Interpretation: 
;; x is the x co-ordinate of the point.
;; y is the y co-ordinate of the point.

;; template:
;; point-fn : Point -> ??
;(define (point-fn p)
;  (... (point-x p) (point-y p)))



;; help function for key event

;; circ-to-point : Circle -> Point
;; GIVEN: a A circle
;; RETURNS: point having co ordinates of center of circle
(define (circ-to-point c)
  (make-point (circ-x c) (circ-y c)))

;; TESTS :
(begin-for-test
  (check-equal? (circ-to-point circle1) (make-point 200 100)
                "it should return point 200 100"))

;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? ke)
  (key=? ke " "))

;; TESTS :
(begin-for-test
  (check-equal? (is-pause-key-event? " ") true
                "it is a pause key event")
  (check-equal? (is-pause-key-event? "q") false
                "it is not a pause key event"))

;; is-new-circle-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a new circle instruction
(define (is-new-circle-key-event? ke)
  (key=? ke "n"))

;; TESTS :
(begin-for-test
  (check-equal? (is-new-circle-key-event? "n") true
                "it is a n key event")
  (check-equal? (is-new-circle-key-event? " ") false
                "it is not a n key event"))


;; is-pen-down-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pen down instruction
(define (is-pen-down-key-event? ke)
  (key=? ke "d"))

;; TESTS :
(begin-for-test
  (check-equal? (is-pen-down-key-event? "d") true
                "it is a d key event")
  (check-equal? (is-pen-down-key-event? " ") false
                "it is not a d key event"))

;; is-pen-up-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pen up instruction
(define (is-pen-up-key-event? ke)
  (key=? ke "u"))

;; TESTS :
(begin-for-test
  (check-equal? (is-pen-up-key-event? "u") true
                "it is a u key event")
  (check-equal? (is-pen-up-key-event? " ") false
                "it is not a u key event"))

;; is-erase-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a erase instruction
(define (is-erase-key-event? ke)
  (key=? ke "e"))

;; TESTS :
(begin-for-test
  (check-equal? (is-erase-key-event? "e") true
                "it is a e key event")
  (check-equal? (is-erase-key-event? " ") false
                "it is not a e key event"))

;; is-up-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents an up instruction
(define (is-up-key-event? ke)
  (key=? ke "up"))

;; TESTS :
(begin-for-test
  (check-equal? (is-up-key-event? "up") true
                "it is a up key event")
  (check-equal? (is-up-key-event? " ") false
                "it is not a up key event"))

;; is-down-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a down instruction
(define (is-down-key-event? ke)
  (key=? ke "down"))

;; TESTS :
(begin-for-test
  (check-equal? (is-down-key-event? "down") true
                "it is a down key event")
  (check-equal? (is-down-key-event? " ") false
                "it is not a down key event"))

;; is-left-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a left instruction
(define (is-left-key-event? ke)
  (key=? ke "left"))

;; TESTS :
(begin-for-test
  (check-equal? (is-left-key-event? "left") true
                "it is a left key event")
  (check-equal? (is-left-key-event? " ") false
                "it is not a left key event"))

;; is-right-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a right instruction
(define (is-right-key-event? ke)
  (key=? ke "right"))

;; TESTS :
(begin-for-test
  (check-equal? (is-right-key-event? "right") true
                "it is a right key event")
  (check-equal? (is-right-key-event? " ") false
                "it is not a right key event"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set

;; EXAMPLES: See test cases

;; STRATEGY: combining simpler functions

(define (initial-world z)
  (make-world empty true initial-mse))

;; tests:
(begin-for-test
  (check-equal? 
    (initial-world 0) 
    (make-world empty true initial-mse)
    "Initial state should be equal to paused state as mentioned."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a worldstate w
;; RETURNS: the worldstate that should follow w after a tick.

;; EXAMPLES: 
;; moving around state:
;; (world-after-tick unpaused-world) = unpaused-world
;; paused state:
;; (world-after-tick paused-world) = paused-world

;; STRATEGY: Use template for World on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (process-world w)))

;; tests:

(begin-for-test
  (check-equal? 
    (world-after-tick paused-world) (make-world (list circle1 circle2) true  initial-mse)
    "in paused world, circles cannot move around")
  (check-equal? 
    (world-after-tick unpaused-world) (make-world
    (list (make-circ 188 120 -12 20 false false initial-drag empty) circle2) false  initial-mse)
    "in unpaused world, circles can move around"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-point-x : Circ -> Int
;; GIVEN: A circ (refer data definations)
;; RETURNS: Next X co-ordinate of the circle.
;; EXAMPLE: (next-point-x (make-circ 10 20 -1 2)) = 9
  
;; STRATEGY: using template of circle.
(define (next-point-x c)
  (+ (circ-x c) (circ-vx c)))
;; TESTS:

(begin-for-test
  (check-equal?
    (next-point-x (make-circ 10 20 -1 2 false false initial-drag empty)) 9 "It should return 9.")
  (check-equal?
    (next-point-x circle2) 188 "It should return 188."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-point-y : Circ -> Int
;; GIVEN: A circ (refer data definations)
;; RETURNS: Next Y co-ordinate of the circle.
;; EXAMPLE: (next-point-y (make-circ 10 20 -1 2)) = 22
  
;; STRATEGY: using template of circle.
(define (next-point-y c)
  (+ (circ-y c) (circ-vy c)))

;; TESTS:

(begin-for-test
  (check-equal?
    (next-point-y (make-circ 10 20 -1 2 false false initial-drag empty))
    22 "It should return 22.")
  (check-equal?
    (next-point-y circle2) 120 "It should return 120."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-right : Circ -> Circ
;; GIVEN: A circ (refer data definations)
;; RETURNS: circle which remains within right hand side boundry.
;; EXAMPLE: (adjust-left-value (make-circ 360 20 20 2)) = 20
  
;; STRATEGY: combining simpler functions.

(define (adjust-right cir)
  (if (circ-pen? cir)
  (make-circ X-RIGHT (next-point-y cir)
                (* -1 (circ-vx cir)) (* 1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir)
                (cons (circ-to-point cir) (circ-points cir)))
  (make-circ X-RIGHT (next-point-y cir)
                (* -1 (circ-vx cir)) (* 1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir) (circ-points cir)))) 


;; TESTS :

(begin-for-test
  
  (check-equal?
    (adjust-right C2) (make-circ 360 90 20 -10 false false initial-drag empty)
    "check for case right-adjust")
  (check-equal?
    (adjust-right C2-pen-active) (make-circ 360 90 20 -10 false true initial-drag
                                            (list (make-point 30 100)))
    "check for case right-adjust"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-left : Circ -> Circ
;; GIVEN: A circ (refer data definations)
;; RETURNS: circle which remains within left hand side boundry.
;; EXAMPLE: (adjust-left (make-circ 360 20 20 2)) = 20
  
;; STRATEGY: combining simpler functions.

(define (adjust-left cir)
  (if (circ-pen? cir)
  (make-circ X-LEFT (next-point-y cir)
                (* -1 (circ-vx cir)) (* 1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir)
                (cons (circ-to-point cir) (circ-points cir)))
  (make-circ X-LEFT (next-point-y cir)
                (* -1 (circ-vx cir)) (* 1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir)
                (circ-points cir))))
;; TESTS :

(begin-for-test
  (check-equal?
    (adjust-left C1) (make-circ 40 90 -20 -10 false false initial-drag empty)
    "check for case left-adjust")
  (check-equal?
    (adjust-left C1-pen-active) (make-circ 40 90 -20 -10 false true initial-drag
                                            (list (make-point 370 100)))
    "check for case right-adjust"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust-up : Circ -> Circ
;; GIVEN: A circ (refer data definations)
;; RETURNS: circle which remains within upper hand side boundry.
;; EXAMPLE: (adjust-up (make-circ 360 20 20 2)) = 20
  
;; STRATEGY: combining simpler functions.

(define (adjust-up cir)
  (if (circ-pen? cir)
  (make-circ (next-point-x cir) Y-UP
               (* 1 (circ-vx cir)) (* -1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir)
               (cons (circ-to-point cir) (circ-points cir)))
  (make-circ (next-point-x cir) Y-UP
               (* 1 (circ-vx cir)) (* -1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir) (circ-points cir))))

;; TESTS :

(begin-for-test
  
  (check-equal?
    (adjust-up C4) (make-circ 220 40 20 -10 false false initial-drag empty)
    "check for case adjust-up")
  (check-equal?
    (adjust-up C4-pen-active) (make-circ 220 40 20 -10 false true initial-drag
                                         (list (make-point 200 280)))
    "check for case adjust-up"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; adjust-down : Circ -> Circ
;; GIVEN: A circ (refer data definations)
;; RETURNS: circle which remains within down side boundry.
;; EXAMPLE: (adjust-down (make-circ 360 20 20 2)) = 20
  
;; STRATEGY: combining simpler functions.
(define (adjust-down cir)
  (if (circ-pen? cir)
  (make-circ (next-point-x cir) Y-DOWN
                (* 1 (circ-vx cir)) (* -1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir)
                (cons (circ-to-point cir) (circ-points cir)))
  (make-circ (next-point-x cir) Y-DOWN
                (* 1 (circ-vx cir)) (* -1 (circ-vy cir)) false (circ-pen? cir) (circ-drag cir) (circ-points cir))))
;; TESTS :


(begin-for-test
  
  (check-equal?
    (adjust-down C3) (make-circ 220 260 20 10 false false initial-drag empty)
    "check for adjust-down")
  (check-equal?
    (adjust-down C3-pen-active) (make-circ 220 260 20 10 false true initial-drag
                                           (list (make-point 200 30)))
    "check for adjust-down"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust : Circ -> Circ
;; GIVEN: A circ (refer data definations)
;; RETURNS: circle which remains within boundry
;;          (used when not hitting any corner).
;; EXAMPLE: (adjust (make-circ circle1)) =
;;     (make-circ 188 120 -12 20)
  
;; STRATEGY: combining simpler functions.

(define (adjust cir)
  (if (circ-pen? cir)
  (make-circ (next-point-x cir) (next-point-y cir) (circ-vx cir)
             (circ-vy cir) (circ-selected? cir) (circ-pen? cir) (circ-drag cir)
             (cons (circ-to-point cir) (circ-points cir)))
  (make-circ (next-point-x cir) (next-point-y cir) (circ-vx cir)
             (circ-vy cir) (circ-selected? cir) (circ-pen? cir) (circ-drag cir) (circ-points cir))))
;; TESTS :

(begin-for-test
  
  (check-equal?
    (adjust circle1) (make-circ 188 120 -12 20 false false initial-drag empty)
    "returns 188 120 -12 20")
  (check-equal?
    (adjust circle1-pen-active) (make-circ 188 120 -12 20 false true initial-drag
                                           (list (make-point 200 100)))
    "returns 188 120 -12 20"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-circle : Circ -> Circ
;; GIVEN: Circ i.e a circle
;; RETURNS: a circle centered with its updated next position and
;;          updated velocities.
;; EXAMPLE: See test case
  
;; STRATEGY: divide into cases on basis of where a bounce occurs.

(define (move-circle c)
  (cond
    [(< (- (next-point-x c) RADIUS) 0) (adjust-left c)]
    [(> (+ (next-point-x c) RADIUS) CANVAS-WIDTH) (adjust-right c)]
    [(> (+ (next-point-y c) RADIUS) CANVAS-HEIGHT) (adjust-down c)]
    [(< (- (next-point-y c) RADIUS) 0) (adjust-up c)]
    [else (adjust c)]))

;; TESTS :


(begin-for-test
  (check-equal?
    (move-circle C1) (make-circ 360 90 -20 -10 false false initial-drag
                                           empty)
    "check for case left")
  (check-equal?
    (move-circle C2) (make-circ 40 90 20 -10 false false initial-drag
                                           empty)
    "check for case right")
  (check-equal?
    (move-circle C3) (make-circ 220 40 20 10 false false initial-drag
                                           empty)
    "check for case down")
  (check-equal?
    (move-circle C4) (make-circ 220 260 20 -10 false false initial-drag
                                           empty)
    "check for case up")
  (check-equal?
    (move-circle circle1) (make-circ 188 120 -12 20 false false initial-drag
                                           empty)
    "check for else case")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; process-world : World -> World
;; GIVEN: WorldState
;; RETURNS: A WorldState with updates in all circles.
;; EXAMPLE: See test case
  
;; STRATEGY: combinig simpler functions.

(define (process-world w)
  (make-world (process-world-helper (world-circles w) empty)
              false (world-mse_pt w) ))

;; TESTS :

(define W1 (make-world (list (make-circ 188 120 -12 20 false false initial-drag empty)
                       (make-circ 200 100 -12 20 true false initial-drag empty))
                       false initial-mse))


(begin-for-test
  (check-equal?
    (process-world unpaused-world) W1 "It should return W1 WorldState.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; process-world-helper : ListofCircles ListofCircles -> ListofCircles
;; GIVEN: list of circles and an empty list
;; RETURNS: list of circles with updated x, y and vx, vy.
;; EXAMPLE: See test case
  
;; STRATEGY: combinig simpler functions.

(define (process-world-helper l1 l2)
  (cond
    [(empty? l1) empty]
    [else
     (if (not (circ-selected? (first l1)))
         (append (cons (move-circle (first l1)) l2) (process-world-helper (rest l1) l2))
         (append (cons (first l1) l2) (process-world-helper (rest l1) l2))
         )]))

(begin-for-test
  (check-equal?
    (process-world-helper (world-circles unpaused-world) empty) (world-circles W1) "It should return W1 circle list.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; new-circle : NonNegInt NonNegInt Int Int -> Circ
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy) and is unselected.
;; EXAMPLE:
;; (new-circ 10 20 -3 -5) = (make-circ 10 20 -3 -5 false)
;; STRATEGY: Create new circle using make-circ
(define (new-circle x y vx vy)
  (make-circ x y vx vy false false initial-drag empty))

;; TESTS:

(begin-for-test
  (check-equal?
    (new-circle 10 20 -3 -5)
    (make-circ 10 20 -3 -5 false false initial-drag empty)
    "It should make a circle with x, y, vx, vy as 10,20,-3,-5, false
      false false respectively."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-circle : Any -> Circ
;; GIVEN: Any value
;; RETURNS: a circle centered at center of canvas and velocity (0, 0)
;; EXAMPLE: See test case
  
;; STRATEGY: Create new circle using make-circ with initial values.

 (define (initial-circle z)
  (make-circ INITIAL-X INITIAL-Y INITIAL-VX
             INITIAL-VY false false initial-drag empty))
;; TEST :

(begin-for-test
  (check-equal?
    (initial-circle 0)
    (make-circ INITIAL-X INITIAL-Y INITIAL-VX
             INITIAL-VY false false initial-drag empty)
    "It should make a circle with x, y, vx, vy as 200,100,-12,20
     respectively."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-pen-down-circle : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after an d button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle


(define (get-pen-down-circle c)
  (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
             (circ-selected? c) true (circ-drag c) (circ-points c)))

;; get-pen-up-circle : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after an u button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-pen-up-circle c)
  (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
             (circ-selected? c) false (circ-drag c) (circ-points c)))

;; get-eraser-circle : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after an e button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-eraser-circle c)
  (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)
             (circ-selected? c) (circ-pen? c) (circ-drag c) empty))

;; get-circle-left-vx : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after a left button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-circle-left-vx c)
  (make-circ (circ-x c) (circ-y c) (- (circ-vx c) 2) (circ-vy c)
             (circ-selected? c) (circ-pen? c)
             (make-drag (circ-x c) (circ-y c) (- (circ-vx c) 2) (circ-vy c))
             (circ-points c)))

;; get-circle-right-vx : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after a right button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-circle-right-vx c)
  (make-circ (circ-x c) (circ-y c) (+ (circ-vx c) 2) (circ-vy c)
             (circ-selected? c) (circ-pen? c)
             (make-drag (circ-x c) (circ-y c) (+ (circ-vx c) 2) (circ-vy c))
             (circ-points c)))

;; get-circle-up-vy : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after a up button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-circle-up-vy c)
  (make-circ (circ-x c) (circ-y c) (circ-vx c) (- (circ-vy c) 2)
             (circ-selected? c) (circ-pen? c)
             (make-drag (circ-x c) (circ-y c) (circ-vx c) (- (circ-vy c) 2))
             (circ-points c)))

;; get-circle-down-vy : Circ -> Circ
;; GIVEN: Circ
;; RETURNS: the state of the given circle after a down button pressed

;; examples: see test cases for circle-after-key-event

;; STRATEGY: using template of Circle

(define (get-circle-down-vy c)
  (make-circ (circ-x c) (circ-y c) (circ-vx c) (+ (circ-vy c) 2)
             (circ-selected? c) (circ-pen? c)
             (make-drag (circ-x c) (circ-y c) (circ-vx c) (+ (circ-vy c) 2))
             (circ-points c)))

;; circle-after-key-event : Circ -> Circ
;; GIVEN: the state of a Circ c
;; RETURNS: the state of the given circle after a tick if it were in an
;; unpaused world.

;; examples: see test cases

;; STRATEGY: divide in cases on basis of key events


(define (circle-after-key-event c kev)
  (cond
    [(is-pen-down-key-event? kev) (get-pen-down-circle c)]
    [(is-pen-up-key-event? kev) (get-pen-up-circle c)]
    [(is-erase-key-event? kev) (get-eraser-circle c)]
    [(is-up-key-event? kev) (get-circle-up-vy c)]
    [(is-down-key-event? kev) (get-circle-down-vy c)]
    [(is-left-key-event? kev) (get-circle-left-vx c)]
    [(is-right-key-event? kev) (get-circle-right-vx c)]
    [else c]
    ))

;; TESTS :

(define cr1 (make-circ 200 100 -12 20 false false initial-drag empty))
(define cr2 (make-circ 200 100 -12 20 false false initial-drag (list (make-point 10 20))))
(define cr1-pen-down (make-circ 200 100 -12 20 false true initial-drag empty))
(define cr1-pen-up (make-circ 200 100 -12 20 false false initial-drag empty))
(define cr1-up (make-circ 200 100 -12 18 false false (make-drag 200 100 -12 18) empty))
(define cr1-down (make-circ 200 100 -12 22 false false (make-drag 200 100 -12 22) empty))
(define cr1-left (make-circ 200 100 -14 20 false false (make-drag 200 100 -14 20) empty))
(define cr1-right (make-circ 200 100 -10 20 false false (make-drag 200 100 -10 20) empty))

(begin-for-test
  (check-equal?
    (circle-after-key-event cr1 "d") cr1-pen-down
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "u") cr1-pen-up
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr2 "e") cr1
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "up") cr1-up
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "down") cr1-down
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "left") cr1-left
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "right") cr1-right
     "It should return second circle")
  (check-equal?
    (circle-after-key-event cr1 "z") cr1
     "It should return second circle"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w and a keyevent kev
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; on space, toggle paused?-- ignore all others
;; EXAMPLES: see tests below
;; STRATEGY: Cases on key events

(define (world-after-key-event w kev)
  (cond
    [(is-pause-key-event? kev) (world-with-paused-toggled w)]
    [(is-new-circle-key-event? kev) (world-with-new-circle-toggled w)] 
    [else (world-after-key-event-helper2 w kev)]
    ))

;; TESTS :

(define paused-world-new-circle
  (make-world (cons (initial-circle 0) list_of_circles) true initial-mse ))

(begin-for-test
  (check-equal?
    (world-after-key-event unpaused-world " ") paused-world
     "It should return second world")
  (check-equal?
    (world-after-key-event paused-world "n") paused-world-new-circle
     "It should return second world")
  (check-equal?
    (world-after-key-event paused-world "q") paused-world
     "It should return second world"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event-helper1 : ListofCircles EmptyList KeyEvent -> ListofCircles
;; GIVEN: list of circles in WorldState
;; RETURNS: ListofCircles with updates according to key events
;; EXAMPLES: see tests below
;; STRATEGY: Cases on list

(define (world-after-key-event-helper1 l1 l2 kev)
  (cond
    [(empty? l1) empty]
    [else
     (if (circ-selected? (first l1))
         (append (cons (circle-after-key-event (first l1) kev) l2)
                 (world-after-key-event-helper1 (rest l1) l2 kev))
         (append (cons (first l1) l2) (world-after-key-event-helper1 (rest l1) l2 kev))
         )]))
;; TESTS :
(begin-for-test
  (check-equal?
    (world-after-key-event-helper1 (world-circles unpaused-world) empty " ") (world-circles paused-world)
     "It should return second circle list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event-helper2 : World KeyEvent -> World
;; GIVEN: a world w and a keyevent kev
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; EXAMPLES: see tests below
;; STRATEGY: use template of World

(define (world-after-key-event-helper2 w kev)
  (make-world (world-after-key-event-helper1 (world-circles w) empty kev) (world-paused? w) (world-mse_pt w)))

;; TESTS :
(begin-for-test
  (check-equal?
    (world-after-key-event-helper2 unpaused-world " ") unpaused-world
     "It should return second world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-circles w) (not (world-paused? w)) (world-mse_pt w)))

;; TESTS :
(begin-for-test
  (check-equal?
    (world-with-paused-toggled unpaused-world) paused-world
     "It should return second world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-with-new-circle-toggled : World -> World
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for World on w
(define (world-with-new-circle-toggled w)
  (make-world
   (cons (initial-circle 0) (world-circles w)) (world-paused? w) (world-mse_pt w)))

;; TESTS :
(begin-for-test
  (check-equal?
    (world-with-new-circle-toggled paused-world) paused-world-new-circle
     "It should return second world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
    (world-after-mouse-helper (world-circles w) empty mx my mev)
    (world-paused? w) (set-mouse-val mx my mev)))

;; TESTS :
(define paused-button-down (make-world list_of_circles true (make-mse 0 0 true) ))
(begin-for-test
  (check-equal?
    (world-after-mouse-event paused-world 0 0 "button-down") paused-button-down
     "It should return second world")
  (check-equal?
    (world-after-mouse-event paused-world 0 0 "drag")
    (make-world (list (make-circ 200 100 -12 20 false false initial-drag empty)
                      (make-circ 0 0 -12 20 true false initial-drag empty)) true
                      (make-mse 0 0 true))
     "It should return second world")
  (check-equal?
    (world-after-mouse-event paused-world 200 100 "button-up")
    (make-world (list (make-circ 200 100 -12 20 false false initial-drag empty)
                      (make-circ 200 100 0 0 false false initial-drag empty)) true
                      (make-mse 100 100 false))
     "It should return second world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-mouse-val mx my mev)
  (if (or (equal? mev "button-down") (equal? mev "drag"))
      (make-mse mx my true)
      (make-mse my my false)))

;; world-after-mouse-helper : ListOfCircles EmptyList Integer Integer MouseEvent -> World
;; GIVEN: ListOfCircles, EmptyList, Integer, Integer, MouseEvent
;; RETURNS: the world that should follow the given mouse event
;; STRATEGY: divide on basis of ListOfCircles

(define (world-after-mouse-helper l1 l2 mx my mev)
  (cond
    [(empty? l1) empty]
    [else
     
         (append (cons (circ-after-mouse-event (first l1) mx my mev) l2)
                 (world-after-mouse-helper (rest l1) l2 mx my mev))
         
         ]))

;; TESTS :

(begin-for-test
  (check-equal?
    (world-after-mouse-helper (world-circles paused-world) empty 0 0 "button-down")
    (world-circles paused-button-down)
     "It should return second world"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; circ-after-mouse-event : Circ Integer Integer MouseEvent -> Circ
;; GIVEN: a circle and a description of a mouse event
;; RETURNS: the circle that should follow the given mouse event
;; examples:  see test cases
;; strategy: Cases on mouse event mev
(define (circ-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (circle-after-button-down c mx my)]
    [(mouse=? mev "drag") (circle-after-drag c mx my)]
    [(mouse=? mev "button-up") (circle-after-button-up c mx my)]
    [else c]))





;; circle-after-button-down : Circ Integer Integer -> Circ
;; Given : Circle and x, y co-ordinate
;; RETURNS: the circle following a button-down at the given location.
;; STRATEGY: Use template for Circ on c
(define (circle-after-button-down c x y)
  (if (check-xy? c x y)
      (make-circ (circ-x c) (circ-y c) (circ-vx c) (circ-vy c) true (circ-pen? c) (circle-to-drag c) (circ-points c))
      c))


;; circle-after-drag : Cat Integer Integer -> Circ
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circ on c
(define (circle-after-drag c x y)
  (if (circ-selected? c)
      (make-circ x y (circ-vx c) (circ-vy c) true (circ-pen? c) (circ-drag c) (circ-points c))
      c))

;; circle-after-button-up : Circ Integer Integer -> Circ
;; RETURNS: the circle following a button-up at the given location
;; STRATEGY: Use template for Circ on c
(define (circle-after-button-up c x y)
  (if (circ-selected? c)
      (make-circ (circ-x c) (circ-y c) (drag-vx (circ-drag c)) (drag-vy (circ-drag c))
                 false (circ-pen? c) (circ-drag c) (circ-points c))
      c))

;; TESTS :

(begin-for-test
  (check-equal?
    (circ-after-mouse-event circle1 0 0 "button-down")
    circle1
     "It should return second circle")
  (check-equal?
    (circ-after-mouse-event circle1 0 0 "button-up")
    circle1
     "It should return second circle")
  (check-equal?
    (circ-after-mouse-event circle1 0 0 "drag")
    circle1
     "It should return second circle"))

;; check-in-range-x : Circ Integer -> Boolean
;; GIVEN: a circle and a x co-ordinate to compare range with.
;; RETURNS: if x co ordinate is within the range
;; examples:  see test cases
;; strategy: combining simpler functions

(define (check-in-range-x? c z)
  
  (and (<= z (+ RADIUS (circ-x c))) (>= z (- (circ-x c) RADIUS))))

;; TESTS :

(begin-for-test
  (check-equal?
    (check-in-range-x? circle1 200 )
    true
     "It should return true"))

;; check-in-range-y : Circ Integer -> Boolean
;; GIVEN: a circle and a y co-ordinate to compare range with.
;; RETURNS: if y co ordinate is within the range
;; examples:  see test cases
;; strategy: combining simpler functions

(define (check-in-range-y? c z)
  
  (and (<= z (+ RADIUS (circ-y c))) (>= z (- (circ-y c) RADIUS))))

;; TESTS :

(begin-for-test
  (check-equal?
    (check-in-range-y? circle1 100 )
    true
     "It should return true"))



;; check-xy? : Circ Integer Integer -> Boolean
;; GIVEN: a circle and a x and y co-ordinate to compare range with.
;; RETURNS: if both x and y co ordinate are within the range
;; examples:  see test cases
;; strategy: combining simpler functions

(define (check-xy? c x y)
  (and (check-in-range-x? c x) (check-in-range-y? c y)))

;; TESTS :

(begin-for-test
  (check-equal?
    (check-xy? circle1 200 100)
    true
     "It should return true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-string : Circ -> String
;; GIVEN: a circle .
;; RETURNS: String containg co-ordinates of center
;; examples:  see test cases
;; strategy: combining simpler functions

(define (set-string c)
  (string-append "(" (number->string (circ-vx c)) "," (number->string (circ-vy c)) ")"))
;; TESTS :

(begin-for-test
(check-equal?
    (set-string circle1)
    "(-12,20)"
     "It should return the String"))

;; Text-getter : Circ -> Image
;; GIVEN: a circle .
;; RETURNS: Image of co ordinates for overlay
;; examples:  see test cases
;; strategy: combining simpler functions

(define (Text-getter c)
  (text (set-string c) 12 "blue"))

;; TESTS :

(begin-for-test
(check-equal?
    (Text-getter circle1)
    (text "(-12,20)" 12 "blue")
     "It should return the image"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; C : Circ -> Image
;; GIVEN: a circle .
;; RETURNS: Image of circle red or blue
;; examples:  see test cases
;; strategy: combining simpler functions

(define (C circle-temp) (circle 40 "outline" (color-circle circle-temp)))

;; TESTS :

(begin-for-test
  (check-equal?
    (C circle1)
    (circle RADIUS "outline" "blue")
     "It should the image"))

;; color-circle : Circ -> Boolean
;; GIVEN: a circle .
;; RETURNS: color of circle i.e red to blue
;; examples:  see test cases
;; strategy: combining simpler functions

(define (color-circle c)
  (cond
    [(equal? (circ-selected? c) true) "red"]
    [(equal? (circ-selected? c) false) "blue"]))

;; TESTS :

(begin-for-test
  (check-equal?
    (color-circle circle1)
    "blue"
     "It should return blue")
  (check-equal?
    (color-circle circle2)
    "red"
     "It should return red"))

;; overlay-circ-text : Circ -> Scene
;; GIVEN: a circle .
;; RETURNS: returns overlay of velocity at center and circle
;; examples:  see test cases
;; strategy: combining simpler functions
(define (overlay-circ-text c-temp)
  (overlay (Text-getter c-temp) (C c-temp)))

;; TESTS :
(begin-for-test
(check-equal?
    (overlay-circ-text circle1)
    (overlay (circle RADIUS "outline" "blue") (text "(-12,20)" 12 "blue"))
     "It should return the image"))

;; process-canvas : ListofCircles Scene -> Scene
;; GIVEN: list of circles and empty scene .
;; RETURNS: scene with all the circles on a canvas as a scene
;; examples:  see test cases
;; strategy: combining simpler functions

(define (process-canvas l canvas)
  (cond
    [(empty? l) canvas]
    [else
     (place-image (overlay-circ-text (first l)) (circ-x (first l)) (circ-y (first l))
                                     (process-canvas (rest l) canvas))]))

;; TESTS :

(define c1 (make-circ 10 20 1 1 false false initial-drag empty))  


(define lx1 (list c1))
(define i1 (place-image (overlay-circ-text (first lx1)) (circ-x (first lx1))
                        (circ-y (first lx1)) EMPTY-CANVAS))

(begin-for-test
(check-equal?
    (process-canvas lx1 EMPTY-CANVAS)
    i1
     "It should return the image"))


;; get-all-points : ListofCircle List -> ListofPoints
;; GIVEN: ListofCircle and empty list .
;; RETURNS: ListofPoints
;; examples:  see test cases
;; strategy: combining simpler functions


(define (get-all-points l1 l2)
  (cond
    [(empty? l1) empty]
    [else
     
         (append (circ-points (first l1)) (get-all-points (rest l1) l2))
         
         ]))
;; TESTS :

(begin-for-test
(check-equal?
    (get-all-points lx1 empty)
    empty
     "It should return the Drag"))

;; process-canvas-helper : ListofCircles Scene -> Scene
;; GIVEN: list of circles and empty scene .
;; RETURNS: scene with all the circles on a canvas as a scene
;; examples:  see test cases
;; strategy: combining simpler functions

(define (process-canvas-helper l canvas)
  (cond
    [(empty? l) canvas]
    [else
     (place-image PEN-DOT (point-x (first l)) (point-y (first l))
                                     (process-canvas-helper (rest l) canvas))]))

;; TESTS :

(define c2 (make-circ 10 20 1 1 false false initial-drag
                      (list (make-point 10 20))))


(define lx2 (list c2))
(define i2 (place-image PEN-DOT 10 20 EMPTY-CANVAS))

(begin-for-test
(check-equal?
    (process-canvas-helper (circ-points (first lx2)) EMPTY-CANVAS)
    i2
     "It should return the image"))

;; put-mouse-dot : Int Int Scene -> Scene
;; GIVEN: x and y co-ordinate and a scene .
;; RETURNS: scene with red dot on x,y co-ordinate
;; examples:  see test cases
;; strategy: combining simpler functions

(define (put-mouse-dot mx my canvas)
  (place-image RED-DOT mx my canvas))

;; TESTS :

(begin-for-test
(check-equal?
    (put-mouse-dot 10 20 EMPTY-CANVAS)
    (place-image RED-DOT 10 20 EMPTY-CANVAS)
     "It should return the image"))

;; pen-down : Int Int Scene -> Scene
;; GIVEN: x and y co-ordinate and a scene .
;; RETURNS: scene with pen dot on x,y co-ordinate
;; examples:  see test cases
;; strategy: combining simpler functions

(define (pen-down mx my canvas)
  (place-image PEN-DOT mx my canvas))

;; TESTS :

(begin-for-test
(check-equal?
    (pen-down 10 20 EMPTY-CANVAS)
    (place-image PEN-DOT 10 20 EMPTY-CANVAS)
     "It should return the image"))

;; circle-to-drag : Circle -> Drag
;; GIVEN: A Circle .
;; RETURNS: a Drag
;; examples:  see test cases
;; strategy: combining simpler functions

(define (circle-to-drag c)
  (make-drag (circ-x c) (circ-y c) (circ-vx c) (circ-vy c)))

;; TESTS :

(begin-for-test
(check-equal?
    (circle-to-drag circle1)
    (make-drag 200 100 -12 20)
     "It should return the Drag"))


;; get-canvas : World -> Scene
;; GIVEN: WorldState .
;; RETURNS: Scene, graphical rep. of World
;; examples:  see test cases
;; strategy: combining simpler functions

(define (get-canvas w)
  (process-canvas (world-circles w)
                  (process-canvas-helper (get-all-points (world-circles w) empty) EMPTY-CANVAS)))

;; get-canvas : World -> Scene
;; GIVEN: WorldState .
;; RETURNS: Scene, graphical rep. of World
;; examples:  see test cases
;; strategy: combining simpler functions

(define (print-canvas w)
  (if (mse-pressed? (world-mse_pt w))
      (put-mouse-dot (mse-mx (world-mse_pt w)) (mse-my (world-mse_pt w))
                     (get-canvas w))
      (get-canvas w)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene (make-world circle1 circle2 true)) =
;;       
;; STRATEGY: Use template for World on w

(define (world-to-scene w)
  (print-canvas w))


;; TESTS :
(define W2 (make-world empty true (make-mse 10 20 true)))
(define W3 (make-world lx1 true (make-mse 10 20 false)))


(begin-for-test
(check-equal?
    (world-to-scene W2)
    (place-image RED-DOT 10 20 EMPTY-CANVAS)
     "It should return the image")
(check-equal?
    (world-to-scene W3)
    (place-image (overlay-circ-text c1) 10 20 EMPTY-CANVAS)
     "It should return the image"))

;; circle-pen-down? : Circle -> Boolean
;; GIVEN : Circle
;; RETURNS: true if the pen in the given circle is down
;; STRATEGY : Using template of circle
;; EXAMPLES : see test case

(define (circle-pen-down? c)
  (circ-pen? c))

;; TESTS :

(begin-for-test
(check-equal?
    (circle-pen-down? c1)
    false
     "It should return false"))

