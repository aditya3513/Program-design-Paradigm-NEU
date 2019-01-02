#lang racket

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

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)   
(require 2htdp/image)

(check-location "09" "q1.rkt")

(provide
 make-metatoy
 run
 make-throbber
 make-clock
 make-politician
 Metatoy<%>
 Toy<%>)

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

;; Data Definitions

;; A Toy is an object whose class implements the Toy<%>
;; interface. 

;; A Metatoy is an object of any class that implements Metatoy<%>.
;; interface. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES


(define World<%>
  (interface ()

    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event

    ; KeyEvent : KeyEvent -> Toy
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;; big-bang will communicate with the metatoy through the Metatoy<%>
;; A Metatoy is an object of any class that implements Metatoy<%>.

(define Metatoy<%>
  (interface 
  
   ;; the (World<%>) says that Metatoy<%> inherits from World<%>
   ;; This means that any class that implements Metatoy<%> must
   ;; implement all the methods from World<%> plus all the methods
   ;; defined here. In this case, there is just one additional method,
   ;; called get-toys.
   (World<%>)

    ;; -> ListOfToy
    get-toys

))


;; A Toy is an object of any class that implements Toy<%>
;; interface.

(define Widget<%>
  (interface ()

    ; -> Toy
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Toy
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Toy
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    to-scene
    ))

;; A Toy is an object of any class that implements Toy<%>
;; Three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
  
   ;; The interface Toy<%> inherits from the interface Widget<%>.
   ;; This means that any class that implements Toy<%> must implement
   ;; all the methods from Widget<%> plus all the methods defined here.
   (Widget<%>)

    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move

 
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run : PosReal -> Metatoy
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs an initial metatoy at the given frame rate
; RETURNS: the final state of the metatoy
(define (run rate)
  (big-bang (initial-metatoy)
    (on-tick
      (lambda (w) (send w after-tick))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (send w after-key-event kev)))
    (on-mouse
      (lambda (w mx my mev)
        (send w after-mouse-event mx my mev)))))

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
      (length (send mt1 get-toys))
      1))"Metatoy after new politician event is incorrect")

;; TEST 3: Checking after-move mouse event.
(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-mouse-event 10 20 MOVE-EVENT)))
    (check-equal?
      (length (send mt1 get-toys))
      0))"Metatoy after after-move mouse event is incorrect")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 after-mouse-event 10 20 DRAG-EVENT)))
    (check-equal?
      (length (send mt1 get-toys))
      0))"Metatoy after after-move mouse event is incorrect")

(begin-for-test
  (local
    ((define mt0 (send (make-metatoy empty) after-key-event NEW-POLITICIAN-EVENT))
     (define mt1 (send mt0 after-tick)))
    (check-equal?
      (length (send mt1 get-toys))
      1))"metatoys after tick test")

(begin-for-test
  (local
    ((define mt0 (send (make-metatoy empty) after-key-event NEW-THROBBER-EVENT))
     (define mt1 (send mt0 after-tick))
     (define mt2 (send mt1 after-mouse-event INITIAL-X INITIAL-Y BUTTON-DOWN-EVENT))
     (define mt3 (send mt1 after-mouse-event INITIAL-X INITIAL-Y BUTTON-UP-EVENT))
     (define mt4 (send mt1 after-mouse-event INITIAL-X INITIAL-Y DRAG-EVENT))
     (define mt5 (send mt1 after-mouse-event INITIAL-X INITIAL-Y MOVE-EVENT))
     (define mt6 (send mt1 after-mouse-event INITIAL-X INITIAL-Y "enter")))
    (check-equal?
      (length (send mt1 get-toys))
      1 "button check 1 metatoy failed")
    (check-equal?
      (length (send mt2 get-toys))
      1 "button check 2 metatoy failed")
    (check-equal?
      (length (send mt3 get-toys))
      1 "button check 3 metatoy failed")
    (check-equal?
      (length (send mt4 get-toys))
      1 "button check 4 metatoy failed")
    (check-equal?
      (length (send mt5 get-toys))
      1 "button check 5 metatoy failed")
    (check-equal?
      (length (send mt6 get-toys))
      1 "button check 6 metatoy failed")))

(begin-for-test
  (local
    ((define mt0 (send (make-metatoy empty) after-key-event NEW-CLOCK-EVENT))
     (define mt1 (send mt0 after-tick))
     (define mt2 (send mt1 after-key-event " ")))
    (check-equal?
      (length (send mt1 get-toys))
      1 "key event check 1 metatoy failed")
    (check-equal?
      (length (send mt2 get-toys))
      1 "key event check 1 metatoy failed")))


(begin-for-test
  (local
    ((define mt0 (send (make-metatoy empty) after-key-event " "))
     (define mt1 (send mt0 after-tick)))
    (check-equal?
      (length (send mt1 get-toys))
      0))"metatoys after tick test")

(begin-for-test
  (local
    ((define mt0 (make-metatoy empty))
     (define mt1 (send mt0 to-scene)))
    (check-equal?
      mt1 EMPTY-CANVAS))
  "metatoy image test failed")

(begin-for-test
  (local
    ((define mt0 (send (make-metatoy empty) after-key-event NEW-THROBBER-EVENT))
     (define mt1 (send mt0 to-scene)))
    (check-equal?
      mt1 (place-image (circle MIN-RADIUS SOLID THROB-COLOR) INITIAL-X INITIAL-Y
                   EMPTY-CANVAS)))
  "metatoy image test failed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Metatoy% class

;; Constructor template for Metatoy% :
;; (new Metatoy% [objs ListOfToy])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its objects as appropriate.


;; make-metatoy : ListOfToy -> Metatoy
;; GIVEN: a list of Toys
;; RETURNS: an object of class Metatoy% containing the given list of
;; Toys.
(define (make-metatoy objs)
  (new Metatoy% [objs objs]))

(define Metatoy%
  (class* object% (Metatoy<%>)

    (init-field objs) ;  ListOfToy

    (super-new)

    ;; after-tick : -> Metatoy
    ;; Use HOFC map on the Toys in this Metatoy

    (define/public (after-tick)
      (make-metatoy
        (map
          (lambda (obj) (send obj after-tick))
          objs)))

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Toys in this Metatoy

    (define/public (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj to-scene scene))
        EMPTY-CANVAS
        objs))

    ;; get-toys : -> ListofToys
    ;; RETURNS : List of toys in metatoy

    (define/public (get-toys)
      objs)


    ;; after-key-event : KeyEvent -> Metatoy
    ;; STRATEGY: Cases on kev
    ;; "t", "c" and "p" create new throbber, new clock and new politican respectively;
    ;; other keystrokes are passed on to the objects in the Metatoy.

    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (make-metatoy
           (cons (make-throbber) objs))]
        [(key=? kev NEW-CLOCK-EVENT)
         (make-metatoy
           (cons (make-clock) objs))]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (make-metatoy
          (cons (make-politician) objs))]
        [else
          (make-metatoy
            (map
              (lambda (obj) (send obj after-key-event kev))
              objs))]))

    ;; metatoy-after-mouse-event : Nat Nat MouseEvent -> Metatoy
    ;; STRATEGY: Cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN-EVENT)
         (metatoy-after-button-down mx my)]
        [(mouse=? mev DRAG-EVENT)
         (metatoy-after-drag mx my)]
        [(mouse=? mev BUTTON-UP-EVENT)
         (metatoy-after-button-up mx my)]
        [(mouse=? mev MOVE-EVENT)
         (metatoy-after-move mx my)]
        [else this]))

    ;; the next few functions are local functions, not in the interface.

    (define (metatoy-after-button-down mx my)
      (make-metatoy
        (map
          (lambda (obj) (send obj after-button-down mx my))
          objs)))
       
    (define (metatoy-after-button-up mx my)
      (make-metatoy
        (map
          (lambda (obj) (send obj after-button-up mx my))
          objs)))

    (define (metatoy-after-drag mx my)
      (make-metatoy
        (map
          (lambda (obj) (send obj after-drag mx my))
          objs)))

    (define (metatoy-after-move mx my)
      (make-metatoy
        (map
          (lambda (obj) (send obj after-move mx my))
          objs)))

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Throbber :

;; checking initial x and y position of throbber and radius as well.
(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-tick)))
    (check-equal?
      (send th1 toy-x)
      250 "throbber check 1 failed")
    (check-equal?
      (send th1 toy-y)
      300 "throbber check 2 failed")
    (check-equal?
      (send th1 toy-data)
      6))"Throbber behavior not as expected")

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-button-down INITIAL-X INITIAL-Y))
     (define th2 (send th0 after-button-down 10 20))
     (define th3 (send th1 after-tick))
     (define th4 (send th0 after-move 10 20)))
    (check-equal?
      (send th1 toy-x)
      250 "throbber check 3 failed")
    (check-equal?
      (send th2 toy-x)
      250 "throbber check 4 failed")
    (check-equal?
      (send th3 toy-x)
      250 "throbber check 5 failed")
    (check-equal?
      (send th4 toy-x)
      250 "throbber check 6 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-button-down INITIAL-X INITIAL-Y))
     (define th2 (send th1 after-drag 10 20))
     (define th3 (send th0 after-drag INITIAL-X INITIAL-Y)))
    (check-equal?
      (send th1 toy-x)
      250 "throbber check 7 failed")
    (check-equal?
      (send th2 toy-x)
      10 "throbber check 8 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-button-up 10 20))
     (define th2 (send th0 after-button-up INITIAL-X INITIAL-Y)))
    (check-equal?
      (send th1 toy-x)
      250 "throbber check 9 failed")
    (check-equal?
      (send th2 toy-x)
      250 "throbber check 10 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-key-event NEW-THROBBER-EVENT)))
    (check-equal?
      (send th1 toy-x)
      250 "throbber check 11 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send (send th0 after-tick) after-tick))
     (define th2 (send (send th1 after-tick) after-tick))
     (define th3 (send (send th2 after-tick) after-tick))
     (define th4 (send (send th3 after-tick) after-tick))
     (define th5 (send (send th4 after-tick) after-tick))
     (define th6 (send (send th5 after-tick) after-tick))
     (define th7 (send (send th6 after-tick) after-tick))
     (define th8 (send (send th7 after-tick) after-tick))
     (define th9 (send (send th8 after-tick) after-tick))
     (define th10 (send (send th9 after-tick) after-tick)))
    (check-equal?
      (send th10 toy-data)
      15 "throbber check 12 failed")
    ))

(begin-for-test
  (local
    ((define th0 (make-throbber))
     (define th1 (send th0 after-button-down INITIAL-X INITIAL-Y))
     (define th2 (send th0 after-button-down 10 20)))
    (check-equal?
      (send th1 to-scene EMPTY-CANVAS)
      (place-image (circle MIN-RADIUS OUTLINE THROB-COLOR) INITIAL-X INITIAL-Y
                   EMPTY-CANVAS)"throbber check 13 failed" )
    (check-equal?
      (send th2 to-scene EMPTY-CANVAS)
      (place-image (circle MIN-RADIUS SOLID THROB-COLOR) INITIAL-X INITIAL-Y
                   EMPTY-CANVAS) "throbber check 14 failed")
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; We have three classes of Toy<%>s: Throbber, Clock and Politician.

;; Throbbers start at the middle of the canvas and can be smoothly dragged.
;; They are selectable and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer]
;;            [selected? Boolean][mx Integer][my Integer])
;; only need to pass x and y coordinates of object of class Throbber%
;; Interpretation: An object of class Throbber% represents a throbber.


(define Throbber%
  (class* object% (Toy<%>)

    ;; the init-fields are the values that may vary from one throbber to
    ;; the next.

    ; the x and y position of the center of the throbber.
    (init-field x y)
   
    ;; flag is a boolean field, representing true means expanding else compressing.
    ;; initially true.
    (init-field [r MIN-RADIUS])

    ;; radius is the radius of the throbber and initially set to MIN-RADIUS.
    ;; initially true.
    (init-field [flag true])

    ; is the throbber selected? Default is false.
    (init-field [selected? false]) 

    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
  

    ; image for displaying the unselected throbber 
    (field [throbber-IMG-UNSELECTED (circle r SOLID THROB-COLOR)])
    ; image for displaying the selected throbber 
    (field [throbber-IMG-SELECTED (circle r OUTLINE THROB-COLOR)])   
       
    (super-new)


    ;; check-inc-or-dec? : PosInt Boolean -> Boolean
    ;; GIVEN: radius of the throbber and a boolean flag
    ;; RETURNS: true iff the throbber size(radius) is increasing
    ;;          after-tick within the given range from 5 px to 20px
    ;; EXAMPLES: (check-inc-or-dec? 6 true) => true
    ;; STRATEGY: Check on the value of radius r
    (define (check-inc-or-dec? r f)
      (cond
        [(equal? r (- MAX-RADIUS OFFSET)) false]
        [(equal? r (+ MIN-RADIUS OFFSET)) true]
        [else f]))

    ;; radius-changer : PosInt Boolean -> PosInt
    ;; GIVEN: radius of the throbber and a boolean flag
    ;; RETURNS: new value of radius after-tick
    ;; EXAMPLES: (radius-changer 10 true) => 11
    ;;           (radius-changer 14 false) => 13
    ;; STRATEGY: Condition on flag f
    (define (radius-changer r f)
      (cond
        [(equal? true f) (+ r OFFSET)]
        [else (- r OFFSET)]))


    ;; after-tick : Time -> Toy
    ;; RETURNS: A throbber like this one, but as it should be after a tick.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
        this
        (new Throbber%
          [x x]
          [y y]
          [flag (check-inc-or-dec? r flag)]
          [r (radius-changer r flag)]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my])))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A metatoy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the throbber 
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
        (new Throbber%
          [x x][y y]
          [r r]
          [flag flag]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)])
        this))

    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the throbber.
    ; If the throbber is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-throbber? mx my)
        (new Throbber%
          [x x][y y]
          [r r] [flag flag]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Throbber%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [r r] [flag flag]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))


    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;;          on it.
    (define/public (to-scene scene)
      (cond
        [(equal? selected? false)
         (place-image throbber-IMG-UNSELECTED x y scene)]
        [else (place-image throbber-IMG-SELECTED x y scene)]))

    ; after-move : Integer Integer -> Toy
    ; GIVEN: the location of a move event
    ; STRATEGY: Combine simpler functions
    ; RETURNS: throbber doesnot respond to move event.
    (define/public (after-move mx my)
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
      r)

    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    ;; STRATEGY: Combine simpler functions
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))

    ))

;; make-throbber: -> Toy
;; GIVEN: no arguments
;; RETURNS: a new object of class throbber% at the middle of the screen.
(define (make-throbber)
  (new Throbber% [x INITIAL-X][y INITIAL-Y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Clock :

;; checking initial x and y position of clock and time as well.
(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-tick)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 1 failed")
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 2 failed")
    (check-equal?
      (send cl1 toy-data)
      1))"Clock behavior not as expected")

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-move 10 20)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 3 failed")
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 4 failed")
    (check-equal?
      (send cl1 toy-data)
      0 "clock check 5 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-key-event NEW-THROBBER-EVENT)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 6 failed")
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 7 failed")
    (check-equal?
      (send cl1 toy-data)
      0 "clock check 8 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-button-down 10 20)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 9 failed")
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 10 failed")
    (check-equal?
      (send cl1 toy-data)
      0 "clock check 11 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-button-down INITIAL-X INITIAL-Y)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 12 failed")
    (check-equal?
      (send cl1 toy-y)
      300 "clock check 13 failed")
    (check-equal?
      (send cl1 toy-data)
      0 "clock check 14 failed")))

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-drag 10 20))
     (define cl2 (send cl0 after-button-up 10 20)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 15 failed")
    (check-equal?
      (send cl2 toy-y)
      300 "clock check 16 failed")
    ))

(begin-for-test
  (local
    ((define cl0 (make-clock))
     (define cl1 (send cl0 after-button-up INITIAL-X INITIAL-Y))
     (define cl2 (send cl0 after-button-down INITIAL-X INITIAL-Y))
     (define cl3 (send cl2 after-drag INITIAL-X INITIAL-Y)))
    (check-equal?
      (send cl1 toy-x)
      250 "clock check 17 failed")
    (check-equal?
      (send cl3 toy-y)
      300 "clock check 18 failed")
    ))

(begin-for-test
  (local
    ((define cl0 (make-clock)))
    (check-equal?
      (send cl0 to-scene EMPTY-CANVAS)
      (place-image (overlay (text (number->string 0) TEXT-SIZE TEXT-COLOR) RECT)
                   INITIAL-X INITIAL-Y EMPTY-CANVAS)
    "clock check 19 failed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Clocks start at the middle of the screen on key-event "c".
;; They are smoothly draggable.

;; Constructor template for Clock%:
;; (new Clock% [x Integer][y Integer]
;;            [selected? Boolean][mx Integer][my Integer])
;; only need to pass x and y coordinates of object of class Clock%
;; Interpretation: An object of class Clock% represents a clock.

(define Clock%
  (class* object% (Toy<%>)

    ;; the init-fields are the values that may vary from one clock to
    ;; the next.

    ; the x and y position of the center of the clock and time of clock
    (init-field x y)

    ; t is the time of the clock, initially t is set to 0.
    (init-field [t INITIAL-TIME]) 

    ; is the clock selected? Default is false.
    (init-field [selected? false]) 

    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock 's center. Else any value.
    (init-field [saved-mx INITIAL-MX] [saved-my INITIAL-MY])

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
  
       
    (super-new)


    ;; get-text : NonNegInt -> Scene
    ;; GIVEN : time i.e number of ticks till start.
    ;; RETURNS: a scene with time as an image.     
    ;; STRATEGY: Combine simpler functions
    (define (get-text time-value)
      (text (number->string time-value) TEXT-SIZE TEXT-COLOR))
    
    
    ;; overlay-clock-text : Scene -> Scene
    ;; GIVEN : an image of a rectangle
    ;; RETURNS: a scene with overlay of time over the rectangle.      
    ;; STRATEGY: Combine simpler functions
    (define (overlay-clock-text time-value)
      (overlay (get-text time-value) RECT))
    
     
    ;; after-tick : Time -> Toy
    ;; RETURNS: A clock  like this one, but as it should be after a tick
    ;;          a selected clock  doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
        (new Clock%
          [x x]
          [y y]
          [t (+ OFFSET t)]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my]))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A metatoy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the clock 
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
        (new Clock%
          [x x][y y]
          [t t]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)])
        this))

    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-clock? mx my)
        (new Clock%
          [x x][y y]
          [t t]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the clock is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Clock%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [t t]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   


    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    (define/public (to-scene scene)
      (place-image (overlay-clock-text t) x y scene))
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    ;; STRATEGY: Combine simpler functions
    (define (in-clock? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr SIDE)))


    ; after-move : Integer Integer -> Toy
    ; GIVEN: the location of a move event
    ; STRATEGY: Combine simpler functions
    ;;RETURNS: clock doesnot respond to move event.
    (define/public (after-move mx my)
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

;; make-clock: -> Toy
;; GIVEN: no arguments
;; RETURNS: a new object of class clock% at the middle of the screen.
(define (make-clock)
  (new Clock% [x INITIAL-X][y INITIAL-Y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for Politician :

;; checking initial x and y position of politician on key event p.
(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-key-event NEW-POLITICIAN-EVENT)))
    (check-equal?
      (send p1 toy-x)
      250 "politician check 1 failed")
    (check-equal?
      (send p1 toy-y)
      300))"Politician behavior after new politician event not as expected")


;; checking initial x and y position of politician on mouse event drag.
(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-move 10 20)))
    (check-equal?
      (send p1 toy-x)
      250 "politician check 2 failed")
    (check-equal?
      (send p1 toy-y)
      300 "politician check 3 failed"))
  "Incorrect initial center of the politician image")



(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-move 250 300)))
    (check-equal?
      (send p1 toy-x)
      250 "politician check 4 failed")
    (check-equal?
      (send p1 toy-y)
      300)
    (check-equal?
      (send p1 toy-data)
      0 "politician check 5 failed")))

(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-tick)))
    (check-equal?
      (send p1 toy-x)
      230.7944680100656 "politician check 6 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send (send p0 after-tick) after-drag INITIAL-X INITIAL-Y))
     (define p2 (send (send p0 after-tick) after-drag 10 20))
     (define p3 (send (send p2 after-drag INITIAL-X INITIAL-Y) after-tick)))
    (check-equal?
      (send p3 toy-x)
      173.17787204026243 "politician check 7 failed")
    (check-equal?
      (send p3 toy-y)
      207.8134464483149 "politician check 8 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-button-up INITIAL-X INITIAL-Y))
     (define p2 (send p0 after-button-up 10 20)))
    (check-equal?
      (send p1 toy-x)
      250 "politician check 9 failed")
    (check-equal?
      (send p2 toy-x)
      250 "politician check 10 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 after-button-down INITIAL-X INITIAL-Y))
     (define p2 (send p0 after-button-down 10 20)))
    (check-equal?
      (send p1 toy-x)
      250 "politician check 11 failed")
    (check-equal?
      (send p2 toy-x)
      250 "politician check 12 failed")
    ))

(begin-for-test
  (local
    ((define p0 (make-politician))
     (define p1 (send p0 to-scene EMPTY-CANVAS)))
    (check-equal?
      p1
      (place-image OBAMA-1 INITIAL-X INITIAL-Y EMPTY-CANVAS)
    "politician check 13 failed")))

(begin-for-test
  (local
    ((define p0 (send (send (make-politician) after-move INITIAL-X INITIAL-Y) after-tick))

     (define p1 (send p0 to-scene EMPTY-CANVAS)))
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Politicians start at the middle of the canvas.
;; They respond to mouse pointer position.

;; Constructor template for Politician%:
;; (new Politician% [x Integer][y Integer]
;;            [near? Boolean][mx Integer][my Integer])
;; only need to pass x and y coordinates of object of class Politician%
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one politiican to
    ;; the next.
    
    ; the x and y position of the center of the politician
    (init-field x y)   
    
    ; is the politician near? Default is false.
    (init-field [near? false])

    ; is image Obama or Putin i.e True represents Obama and false represents Putin.
    ;; initially true.
    (init-field [image-obama? true])
    
    ;; if the politician is near, the position of
    ;; the last button-down event inside the politician, relative to the
    ;; politician's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    
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
    (define (distance)
      (sqrt (+ (sum-square x saved-mx) (sum-square y saved-my))))

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
    
    
    
    ;; after-tick : Time -> Toy
    ;; RETURNS: A politician like this one, but as it should be after a tick
    ;; a near politician doesn't move.
    ;; STRATEGY: Cases on near?
    (define/public (after-tick)
      (new Politician%
           [x (next-point x saved-mx (distance)
                          (near-mouse? x y saved-mx saved-my))]
           [y (next-point y saved-my (distance)
                          (near-mouse? x y saved-mx saved-my))]
           [image-obama? (xor image-obama? near?)]
           [near? (near-mouse? x y saved-mx saved-my)]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A metatoy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a politician ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the politician
    (define/public (after-button-down mx my)
      (after-mouse-event-updator mx my))
    
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the politician.
    ; If the politician is near, then unselect it.
    (define/public (after-button-up mx my)
      (after-mouse-event-updator mx my))      
    
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the politician is near.
    ; If it is near, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (after-mouse-event-updator mx my))

    ; after-mouse-event-updator : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the politician is near.
    ; If it is near, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-mouse-event-updator mx my)
      (if (near-mouse? x y mx my)
          (new Politician%
               [x x]
               [y y]
               [near? true]
               [image-obama? image-obama?]
               [saved-mx mx]
               [saved-my my])
          (new Politician%
               [x x]
               [y y]
               [near? false]
               [image-obama? image-obama?]
               [saved-mx mx]
               [saved-my my])))
    
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this politician painted
    ;; on it.
    (define/public (to-scene scene)
      (if (equal? image-obama? true)
          (place-image OBAMA-1 x y scene)
          (place-image PUTIN-1 x y scene)))
    
    ;; near-mouse? : Integer Integer -> Boolean
    ;; GIVEN: a location on the politician on the canvas
    ;; RETURNS: true iff the location is near this mouse pointer.
    ;; STRATEGY: Combine simpler functions
    (define (near-mouse? other-x other-y mx my)
      (or (<= (+ (sqr (- mx other-x)) (sqr (- my other-y)))
          (sqr POINTER-RADIUS))
          (<= (+ (sqr (- saved-mx other-x)) (sqr (- saved-my other-y)))
          (sqr POINTER-RADIUS))))


    
    ; after-move : Integer Integer -> Toy
    ; GIVEN: the location of a move event
    ; STRATEGY: Combine simpler functions
    ;;RETURNS: the state of politician that should follow a mouse-move
    ;;  at the given coordinates
    (define/public (after-move mx my)
      (after-mouse-event-updator mx my))

    ;; toy-x : -> Int
    ;; RETURNS: the x position of the center of the politician
    (define/public (toy-x)
      x)

    ;; toy-y : -> Int
    ;; RETURNS: the y position of the center of the politician
    (define/public (toy-y)
      y)

    ;; toy-data : -> Int
    ;; RETURNS: the current distance to the mouse
    (define/public (toy-data)
      (distance))
    
    ))

;; make-politician: -> Toy
;; GIVEN: no arguments
;; RETURNS: a new object of class Politician%

(define (make-politician)
  (new Politician% [x INITIAL-X][y INITIAL-Y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-metatoy : -> Metatoy
;; RETURNS: a world no Toys initially.
(define (initial-metatoy)
  (make-metatoy empty))

;; TESTS :


(begin-for-test
  (local
    ((define mt0 (initial-metatoy)))
    (check-equal?
      (length (send mt0 get-toys))
      0 "initial-metatoy check failed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



