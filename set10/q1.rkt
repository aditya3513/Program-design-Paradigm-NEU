#lang racket
(require 2htdp/universe)   
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with (run framerate). Typically: (run 0.4)

;; Press "t" to add a new throbber mini-toy.  
;; Throbber increases from 5 px radius to 20 px radius every tick
;; then decreases at every tick till it reaches 5 px radius
;; and the cycles continues.
;; Throbber is smoothly draggable.

;; Press "c" to add a new clock mini-toy.
;; Clock displays the number of ticks since it was created.
;; Clock is smoothly draggable.

;; Press "p" to add a new politician mini-toy.
;; Politician always moves in a straight line either
;; towards the mouse position or away from it.
;; However, he never reaches or passes the mouse position.
;; When the center of the politician is at least
;; 75 pixels away from the mouse position,
;; the politician moves towards the mouse.
;; Politicians are two-faced meaning the image changes
;; when it moves away from the mouse position.

;; there is no interaction between the throbber, clock and politician.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; some arbitrary choices
(define INITIAL-X (/ CANVAS-WIDTH 2))  
(define INITIAL-Y (/ CANVAS-HEIGHT 2))
(define SIDE 80)
(define POINTER-RADIUS 75)
(define RECT (rectangle SIDE (/ SIDE 2)  "outline" "blue"))

(define OBAMA-1 (bitmap "obama.jpg"))
(define PUTIN-1 (bitmap "putin.jpg"))

(define MIN-RADIUS 5)
(define MAX-RADIUS 20)

(define NORMALIZATION-OFFSET 16)

(define APPROACH-OFFSET 30)
(define RUNNING-OFFSET 90)

(define OFFSET 1)

(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")
(define TEXT-SIZE 15)
(define TEXT-COLOR "red")
(define INITIAL-MX 0)
(define INITIAL-MY 0)
(define INITIAL-TIME 0)
(define SOLID "solid")
(define OUTLINE "outline")
(define THROB-COLOR "green")
(define BUTTON-DOWN-EVENT "button-down")
(define BUTTON-UP-EVENT "button-up")
(define DRAG-EVENT "drag")
(define MOVE-EVENT "move")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Toy is an object whose class implements the SWidget<%>
;; interface. 

;; A Metatoy is an object whose class implements SWidget<%>.
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; A Metatoy is an object of any class that implements Metatoy<%>.

(define Metatoy<%>
  (interface 
      
      ;; the (SWidget<%>) says that Metatoy<%> inherits from SWidget<%>
      ;; This means that any class that implements Metatoy<%> must
      ;; implement all the methods from SWidget<%> plus all the methods
      ;; defined here. In this case, there is just one additional method,
      ;; called get-toys.
      (SWidget<%>)
    
    ;; -> ListOfToy
    get-toys
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Toy is an object of any class that implements SWidget<%>
;; Three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface SWidget<%>.
      ;; This means that any class that implements Toy<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of this toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    
    ))


;; We have three classes of Toy<%>s: Throbber, Clock and Politician.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : -> WorldState

(define (initial-world)
  (local
    ((define the-metatoy
       (make-metatoy empty)))
    (begin
      ;; put the metatoy in the world
      (send (send the-metatoy get-world) add-stateful-widget the-metatoy)
      (send the-metatoy get-world))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; run : PosReal -> World
(define (run rate)
  (send (initial-world) run rate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The Metatoy% class
;
;;; Constructor template for Metatoy% :
;;; (new Metatoy% [world Container%] [objs ListOfToy])
;;; Interpretation:
;; objs is the list of toys in the metatoy.
;; world is the container in which all SWidgets are added.

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field [world (container-init CANVAS-WIDTH CANVAS-HEIGHT)])  
    (init-field [objs empty])
    
    
    
    
    (super-new)


    ; KeyEvent -> SWidget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    ;; DESIGN STRATEGY : divide by cases on value of kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (local
           ((define t (make-throbber INITIAL-X INITIAL-Y)))
           (begin (set! objs (cons t objs)))
           (send world add-stateful-widget t))]
        [(key=? kev NEW-CLOCK-EVENT)
         (local
           ((define c (make-clock INITIAL-X INITIAL-Y)))
           (begin (set! objs (cons c objs)))
           (send world add-stateful-widget c))]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (local
           ((define p (make-politician INITIAL-X INITIAL-Y)))
           (begin (set! objs (cons p objs)))
           (send world add-stateful-widget p))]))

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    (define/public (after-tick) this)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this swidget to the state it should have
    ; following the mouse button down event at given location
    (define/public (after-button-down mx my) this)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this swidget to the state it should have
    ; following the mouse button up event at given location
    (define/public (after-button-up mx my) this)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this swidget to the state it should have
    ; following the mouse move event at given location
    (define/public (after-move mx my) this)

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this swidget to the state it should have
    ; following the mouse drag event at given location.
    (define/public (after-drag mx my) this)

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this metatoy
    ; painted on it.
    (define/public (add-to-scene s) s)

    ;; -> ListOfToy
    ;; GIVEN : takes no agrument
    ;; RETURNS :  list of Toys in this metatoy
    (define/public (get-toys)
      objs)

    ;; -> Container
    ;; GIVEN : takes no agrument
    ;; RETURNS :  the container in which all the SWidgets are present.
    (define/public (get-world)
      world)
    
    
    ))


;; make-metatoy : ListOfToy -> Metatoy
;; GIVEN: a list of toys
;; RETURNS: an object of class Metatoy% containing the given list of
;; Toys.
(define (make-metatoy objs)
  (new Metatoy% [objs objs]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Throbber% class : 

;; Throbbers start at the middle of the canvas and can be smoothly dragged.
;; They are selectable and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer][radius PosInt] [flag Boolean]
;;            [selected? Boolean][saved-mx Integer][saved-my Integer])
;; only need to pass x and y coordinates of object of class Throbber%

;; Interpretation: An object of class Throbber% represents a throbber.

(define Throbber%
  (class* object% (Toy<%>)
    
    
    
    ;; the init-fields are the values that may vary from one throbber to
    ;; the next.
    
    ; the x and y position of the center of the throbber.
    (init-field [x INITIAL-X])
    (init-field [y INITIAL-Y])
    
    ;; flag is a boolean field, representing true means expanding else compressing.
    ;; initially true.
    (init-field [flag true])
    
    
    ; is this selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; radius is the radius of the throbber and initially set to MIN-RADIUS.
    ;; initially true.
    (init-field [radius MIN-RADIUS])
    
    ; image for displaying the unselected throbber 
    (field [throbber-IMG-UNSELECTED (circle radius SOLID THROB-COLOR)])
    ; image for displaying the selected throbber 
    (field [throbber-IMG-SELECTED (circle radius OUTLINE THROB-COLOR)])   
    
    (super-new)
    
    
    ;; check-inc-or-dec? : -> Boolean
    ;; RETURNS: true iff the throbber size(radius) is increasing
    ;;          after-tick within the given range from 5 px to 20px
    ;; STRATEGY: divide by cases by values of radius and flag
    (define (check-inc-or-dec?)
      (cond
        [(equal? radius (- MAX-RADIUS OFFSET)) false]
        [(equal? radius (+ MIN-RADIUS OFFSET)) true]
        [else flag]))
    
    
    ;; radius-changer : -> PosInt
    ;; RETURNS: new value of radius after-tick
    ;; STRATEGY: divide by cases by values of flag
    (define (radius-changer)
      (cond
        [(equal? true flag) (+ radius OFFSET)]
        [else (- radius OFFSET)]))
    
    
    
    ;; after-tick : -> Void
    ;; RETURNS: A throbber like this one, but as it should be after a tick.
    (define/public (after-tick)
      (let ((r (+ radius 1)))
        (begin
          (set! flag (check-inc-or-dec?))
          (set! radius (radius-changer)))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;;          on it.
    (define/public (add-to-scene s)
      (if (equal? selected? true)
          (place-image
           (circle radius OUTLINE THROB-COLOR) x y s)
          (place-image
           (circle radius SOLID THROB-COLOR) x y s)))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the throbber 
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-this? mx my)
          (set! selected? false)
          'error-276))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          this))
    
    ; after-move : Integer Integer -> Void
    ; GIVEN: the location of a move event
    (define/public (after-move mx my)
      this) 
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A metatoy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev)
      this)
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the throbber
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the throbber
    (define/public (toy-y)
      y)
    
    ;; toy-data : -> Int
    ;; RETURNS: the current radius of the throbber
    (define/public (toy-data)
      radius)
    
    ))


;; make-throbber: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x][y y]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Clocks start at the middle of the screen on key-event "c".
;; They are smoothly draggable.

;; Constructor template for Clock%:
;; (new Clock% [x Integer][y Integer][t PosInt]
;;            [selected? Boolean][saved-mx Integer][saved-my Integer])
;; only need to pass x and y coordinates of object of class Clock%
;; Interpretation: An object of class Clock% represents a clock.

(define Clock%
  (class* object% (Toy<%>)
    
    
    
    ;; the init-fields are the values that may vary from one clock to
    ;; the next.
    
    ; the x and y position of the center of the clock and time of clock
    (init-field [x INITIAL-X])
    (init-field [y INITIAL-Y])
    
    ; t is the time of the clock, initially t is set to 0.
    (init-field [t INITIAL-TIME]) 
    
    
    ; is the clock selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock 's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    
    (super-new)
    
    
    ;; get-text : PosInt -> Scene
    ;; GIVEN : time i.e number of ticks till start.
    ;; RETURNS: a scene with time as an image.     
    ;; STRATEGY: Combine simpler functions
    (define (get-text time-value)
      (text (number->string time-value) TEXT-SIZE TEXT-COLOR))
    
    
    ;; overlay-clock-text : PosInt -> Scene
    ;; GIVEN : an image of a rectangle
    ;; RETURNS: a scene with overlay of time over the rectangle.      
    ;; STRATEGY: Combine simpler functions
    (define (overlay-clock-text time-value)
      (overlay (get-text time-value) RECT))
    
    
    
    ;; after-tick : -> Void
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (begin
        (set! t (+ t OFFSET))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image (overlay-clock-text t) x y scene))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the clock 
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ;; in-this? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this.
    (define (in-this? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr SIDE)))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-this? mx my)
          (set! selected? false)
          'error-276))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the clock is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))
    
    ; after-move : Integer Integer -> Void
    ; GIVEN: the location of a move event
    ; STRATEGY: Combine simpler functions
    ; RETURNS: clock doesnot respond to move event.
    (define/public (after-move mx my)
      this) 
    
    ;; after-key-event : KeyEvent -> Void
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the clock
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the clock
    (define/public (toy-y)
      y)
    
    ;; toy-data : -> Int
    ;; RETURNS: the current value of the clock
    (define/public (toy-data)
      t)
    
    ))

;; make-clock : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.

(define (make-clock x y)
  (new Clock% [x x][y y]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Politicians start at the middle of the canvas.
;; They respond to mouse pointer position.

;; Constructor template for Politician%:
;; (new Politician% [x Integer][y Integer] [flag Boolean] [image-obama? Boolean]
;;            [near? Boolean][mx Integer][my Integer])
;; only need to pass x and y coordinates of object of class Politician%
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    
    
    ;; the init-fields are the values that may vary from one politiican to
    ;; the next.
    
    ; the x and y position of the center of the politician
    (init-field [x INITIAL-X])
    (init-field [y INITIAL-Y])
    
    ;; flag is a boolean field, representing true means expanding else compressing.
    ;; initially true.
    (init-field [flag true])
    
    
    ; is the politician near? Default is false.
    (init-field [near? false])
    
    ;; if the politician is near, the position of
    ;; the last button-down event inside the politician, relative to the
    ;; politician's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ; is image Obama or Putin i.e True represents Obama and false represents Putin.
    ;; initially true.
    (init-field [image-obama? true])
    
    
    
    (super-new)
    
    
    ;; sum-square : PosInt PosInt -> PosInt
    ;; GIVEN: values of x and y
    ;; RETURNS: the square of the difference between x and y
    ;; EXAMPLES: (sum-square 3 5) => 4
    ;; STRATEGY: Combine simpler functions
    (define (sum-square x y)
      (sqr (- x y)))
    
    ;; distance : PosInt PosInt PosInt PosInt -> PosReal
    ;; GIVEN: two points in the plane
    ;; RETURNS: the distance between the points
    ;; EXAMPLES: (distance 4 3 7 7) => 5
    ;; STRATEGY: Combine simpler functions
    (define (distance x1 y1 x2 y2)
      (sqrt (+ (sum-square x1 x2) (sum-square y1 y2))))
    
    ;; unit-vector : PosInt PosInt PosReal -> Real
    ;; GIVEN: two points and distance between two points
    ;; RETURNS: a unit-vector for the given point
    ;; EXAMPLES: (unit-vector 3 4 5) => 1/5
    ;; STRATEGY: Combine simpler functions
    (define (unit-vector x1 x2 d)
      (if (equal? d 0)
          0
          (/ (- x2 x1) d)))
    
    ;; next-point-forward : PosInt PosInt PosReal -> Real
    ;; GIVEN: two points x1 and x2, distance d
    ;; RETURNS: a point that follows the given point
    ;; EXAMPLES: (next-point-forward 3 4 5) => 9
    ;; STRATEGY: Combine simpler functions
    (define (next-point-forward x1 x2 d)
      (+ x1 (* APPROACH-OFFSET (unit-vector x1 x2 d))))
    
    ;; next-point-backward : PosInt PosInt PosReal -> Real
    ;; GIVEN: two points x1 and x2, distance d
    ;; RETURNS: a point that follows the given point
    ;; EXAMPLES: (next-point-backward 3 4 5) => (-15)
    ;; STRATEGY: Combine simpler functions
    (define (next-point-backward x1 x2 d)
      (- x1 (* RUNNING-OFFSET (unit-vector x1 x2 d))))
    
    ;; next-point : PosInt PosInt PosReal Boolean -> Real
    ;; GIVEN: two points x1 and x2, distance d and boolean near-mouse?
    ;; RETURNS: the point that follows the given point
    ;; EXAMPLES: (next-point 3 4 5 false) => 9
    ;;           (next-point 3 4 5 true) => (-15)
    ;; STRATEGY: Combine simpler fucntions
    (define (next-point x1 x2 d near-mouse?)
      (cond
        [(equal? near-mouse? false) (next-point-forward x1 x2 d)]
        [else (next-point-backward x1 x2 d)]))
    
    
    
    ;; after-tick : -> Void
    ;; RETURNS: A throbber like this one, but as it should be after a tick.
    (define/public (after-tick)
      (let ((next-x (next-point x saved-mx (distance x y saved-mx saved-my)
                                (near-mouse? x y saved-mx saved-my)))
            (next-y (next-point y saved-my (distance x y saved-mx saved-my)
                                (near-mouse? x y saved-mx saved-my))))
        (begin
          (set! x next-x)
          (set! y next-y)
          (set! image-obama? (xor image-obama? near?))
          (set! near? (near-mouse? x y saved-mx saved-my)))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;;          on it.
    (define/public (add-to-scene scene)
      (if (equal? image-obama? true)
          (place-image OBAMA-1 x y scene)
          (place-image PUTIN-1 x y scene)))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the throbber 
    (define/public (after-button-down mx my)
      (if (near-mouse? x y mx my)
          (begin
            (set! near? true)
            (set! saved-mx mx)
            (set! saved-my my))
          (begin
            (set! near? false)
            (set! saved-mx mx)
            (set! saved-my my))))
    
    ;; near-mouse? : Integer Integer -> Boolean
    ;; GIVEN: a location on the politician on the canvas
    ;; RETURNS: true iff the location is near this mouse pointer.
    ;; STRATEGY: Combine simpler functions
    (define (near-mouse? other-x other-y mx my)
      (or (<= (+ (sqr (- mx other-x)) (sqr (- my other-y)))
              (sqr POINTER-RADIUS))
          (<= (+ (sqr (- saved-mx other-x)) (sqr (- saved-my other-y)))
              (sqr POINTER-RADIUS))))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (after-button-down mx my))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (after-button-down mx my))
    
    ; after-move : Integer Integer -> Void
    ; GIVEN: the location of a move event
    (define/public (after-move mx my)
      (after-button-down mx my)) 
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A metatoy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev)
      this)
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the throbber
    (define/public (toy-x)
      x)
    
    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the throbber
    (define/public (toy-y)
      y)
    
    ;; toy-data : -> Int
    ;; RETURNS: the current radius of the throbber
    (define/public (toy-data)
      (distance x y saved-mx saved-my))
    
    ))

;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position.

(define (make-politician x y)
  (new Politician% [x x][y y]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Metatoy :

;; TEST 1: Checking after-tick, no key pressed.
(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-tick)))
    (check-equal?
      (length (send mt1 get-toys))
      0))"Metatoy after-tick is incorrect")

;; TEST 2: Checking after "p" key pressed.
(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-key-event NEW-POLITICIAN-EVENT)))
    (check-equal?
      (length (send mt0 get-toys))
      1))"Metatoy after new politician event is incorrect")

;; TEST 3: Checking after-move mouse event.
(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-move 10 20)))
    (check-equal?
      (length (send mt0 get-toys))
      0))"Metatoy after after-move mouse event is incorrect")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-drag 10 20)))
    (check-equal?
      (length (send mt0 get-toys))
      0))"Metatoy after after-move mouse event is incorrect")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))
    (send mt0 after-key-event NEW-POLITICIAN-EVENT)
    (send mt0 after-tick)
    (check-equal?
      (length (send mt0 get-toys))
      1))"metatoys after tick test")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))

    (send mt0 after-key-event NEW-THROBBER-EVENT)
    (send mt0 after-tick)
    (check-equal?
      (length (send mt0 get-toys))
      1 "button check 1 metatoy failed")
    
    (send mt0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (length (send mt0 get-toys))
      1 "button check 2 metatoy failed")

    (send mt0 after-button-up INITIAL-X INITIAL-Y)
    (check-equal?
      (length (send mt0 get-toys))
      1 "button check 3 metatoy failed")

    (send mt0 after-drag INITIAL-X INITIAL-Y)
    (check-equal?
      (length (send mt0 get-toys))
      1 "button check 4 metatoy failed")

    (send mt0 after-move INITIAL-X INITIAL-Y)
    (check-equal?
      (length (send mt0 get-toys))
      1 "button check 5 metatoy failed")

    ;(send mt0 after-mouse-event INITIAL-X INITIAL-Y "enter")
    ;(check-equal?
    ;  (length (send mt0 get-toys))
     ; 1 "button check 6 metatoy failed")
    ))

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))
    (send mt0  after-key-event NEW-CLOCK-EVENT)
    (send mt0 after-tick)

    (check-equal?
      (length (send mt0 get-toys))
      1 "key event check 1 metatoy failed")

        (send mt0 after-key-event " ")
    (check-equal?
      (length (send mt0 get-toys))
      1 "key event check 1 metatoy failed")))


(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))
    (send mt0 after-key-event " ")
    (send mt0 after-tick)
    (check-equal?
      (length (send mt0 get-toys))
      0))"metatoys after tick test")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))

     (define s (send mt0 add-to-scene EMPTY-CANVAS))
    (check-equal?
      s EMPTY-CANVAS))
  "metatoy image test failed")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty)))
    (send mt0 after-key-event NEW-THROBBER-EVENT)
    (define s (send mt0 add-to-scene EMPTY-CANVAS))
    (check-equal?
      s EMPTY-CANVAS))
  "metatoy image test failed")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Throbber :

;; checking initial x and y position of throbber and radius as well.
(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))
    (send th0 after-tick)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 1 failed")
    (check-equal?
      (send th0 toy-y)
      300 "throbber check 2 failed")
    (check-equal?
      (send th0 toy-data)
      6))"Throbber behavior not as expected")

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 3 failed")

    (send th0 after-button-down 10 20)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 4 failed")

    (send th0 after-tick)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 5 failed")

    (send th0 after-move 10 20)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 6 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 7 failed")

    (send th0 after-drag 10 20)
    (check-equal?
      (send th0 toy-x)
      10 "throbber check 8 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-button-up 10 20)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 9 failed")

    (send th0 after-button-up INITIAL-X INITIAL-Y)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 10 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-key-event NEW-THROBBER-EVENT)
    (check-equal?
      (send th0 toy-x)
      250 "throbber check 11 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-tick) (send th0 after-tick)
    (send th0 after-move INITIAL-X INITIAL-Y)
    (send th0 after-drag INITIAL-MX INITIAL-MY)
    
    (check-equal?
      (send th0 toy-data)
      15 "throbber check 12 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber INITIAL-X INITIAL-Y)))

    (send th0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (send th0 add-to-scene EMPTY-CANVAS)
      (place-image (circle MIN-RADIUS OUTLINE THROB-COLOR) INITIAL-X INITIAL-Y
                   EMPTY-CANVAS)"throbber check 13 failed" )

    (send th0 after-button-up INITIAL-X INITIAL-Y)
    (send th0 after-button-down 10 20)
    (check-equal?
      (send th0 add-to-scene EMPTY-CANVAS)
      (place-image (circle MIN-RADIUS SOLID THROB-COLOR) INITIAL-X INITIAL-Y
                   EMPTY-CANVAS) "throbber check 14 failed")
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Clock :

;; checking initial x and y position of clock and time as well.
(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
    (send cl0 after-tick)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 1 failed")
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 2 failed")
    (check-equal?
      (send cl0 toy-data)
      1))"Clock behavior not as expected")

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
     (send cl0 after-move 10 20)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 3 failed")
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 4 failed")
    (check-equal?
      (send cl0 toy-data)
      0 "clock check 5 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
    (send cl0 after-key-event NEW-THROBBER-EVENT)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 6 failed")
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 7 failed")
    (check-equal?
      (send cl0 toy-data)
      0 "clock check 8 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
     (send cl0 after-button-down 10 20)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 9 failed")
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 10 failed")
    (check-equal?
      (send cl0 toy-data)
      0 "clock check 11 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
    (send cl0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 12 failed")
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 13 failed")
    (check-equal?
      (send cl0 toy-data)
      0 "clock check 14 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))

    (send cl0 after-drag 10 20)
    (check-equal?
      (send cl0 toy-x)
      250 "clock check 15 failed")

    (send cl0 after-button-up 10 20)
    (check-equal?
      (send cl0 toy-y)
      300 "clock check 16 failed")
    ))

(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y))
     (define cl1 (make-clock INITIAL-X INITIAL-Y)))


     (send cl0 after-button-up INITIAL-X INITIAL-Y)
     (check-equal?
      (send cl0 toy-x)
      250 "clock check 17 failed")

     (send cl1 after-button-down INITIAL-X INITIAL-Y)
     (send cl1 after-drag INITIAL-X INITIAL-Y)
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 18 failed")
    ))


(begin-for-test
  (local
    ((define cl0 (make-clock INITIAL-X INITIAL-Y)))
    (check-equal?
      (send cl0 add-to-scene EMPTY-CANVAS)
      (place-image (overlay (text (number->string 0) TEXT-SIZE TEXT-COLOR) RECT)
                   INITIAL-X INITIAL-Y EMPTY-CANVAS)
    "clock check 19 failed")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Politician :

;; checking initial x and y position of politician on key event p.
(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (send p0 after-key-event NEW-POLITICIAN-EVENT)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 1 failed")
    (check-equal?
      (send p0 toy-y)
      300))"Politician behavior after new politician event not as expected")


;; checking initial x and y position of politician on mouse event drag.
(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (send p0 after-move 10 20)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 2 failed")
    (check-equal?
      (send p0 toy-y)
      300 "politician check 3 failed"))
  "Incorrect initial center of the politician image")



(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (send p0 after-move 250 300)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 4 failed")
    (check-equal?
      (send p0 toy-y)
      300)
    (check-equal?
      (send p0 toy-data)
      0 "politician check 5 failed")))

(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (send p0 after-tick)
    (check-equal?
      (send p0 toy-x)
      230.7944680100656 "politician check 6 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (send p0 after-tick)
    (send p0 after-drag INITIAL-X INITIAL-Y)
    (send p0 after-tick)
    (send p0 after-drag 10 20)
    (send p0 after-drag INITIAL-X INITIAL-Y)
    (send p0 after-tick)
    
    (check-equal?
      (send p0 toy-x)
      192.38340403019683 "politician check 7 failed")
    (check-equal?
      (send p0 toy-y)
      230.86008483623618 "politician check 8 failed")
    ))


(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y))
     (define p1 (make-politician INITIAL-X INITIAL-Y)))


    (send p0 after-button-up INITIAL-X INITIAL-Y)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 9 failed")

    (send p1 after-button-up 10 20)
    (check-equal?
      (send p1 toy-x)
      250 "politician check 10 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))


    (send p0 after-button-down INITIAL-X INITIAL-Y)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 11 failed")

    (send p0 after-button-down 10 20)
    (check-equal?
      (send p0 toy-x)
      250 "politician check 12 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))

    (check-equal?
      (send p0 add-to-scene EMPTY-CANVAS)
      (place-image OBAMA-1 INITIAL-X INITIAL-Y EMPTY-CANVAS)
    "politician check 13 failed")))



(begin-for-test
  (local
    ((define p0 (make-politician INITIAL-X INITIAL-Y)))
    (send p0 after-move INITIAL-X INITIAL-Y)
    (send p0 after-tick)

     (define p1 (send p0 add-to-scene EMPTY-CANVAS))
    (check-equal?
      (send p0 toy-x)
      250 "politician check 14 failed")
    (check-equal?
      (send p0 toy-y)
      300 "politician check 15 failed")
    (check-equal?
      p1
      (place-image PUTIN-1 250 300 EMPTY-CANVAS)
    "politician check 16 failed")
    ))



