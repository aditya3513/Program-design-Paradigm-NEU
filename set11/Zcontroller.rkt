#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PerfectBounce.rkt")

(provide
 ZController%
 make-z-controller)


(define RED-CIRCLE (circle 10 "solid" "red"))
(define BLACK-DOT (circle 2 "solid" "black"))
(define BALL (overlay/align "middle" "middle" BLACK-DOT RED-CIRCLE))

;; make-z-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A z controller for m

(define (make-z-controller m)
  (new ZController% [model m]))


;; a ZController% is a (new ZController% [model Model<%>])
;; inherits XYController%

(define ZController%
  (class* object% (Controller<%>)

    ;; the model
    (init-field model)
    
    ; Position of the center of the controller
    ; both of these are NonNegInts.
    (init-field [x 100] [y 100])   
    
    ;; canvas boundary in which the particle bounces.  Both PosInts.
    (init-field [width 150][height 100])

    ;; half the width of the contoller is saved here. (PosInt)
    (field [half-width  (/ width  2)])

    ;; half the height of the contoller is saved here. (PosInt)
    (field [half-height (/ height 2)])

    ;; the center for calculating distnace (a reference point x co ordinate)
    ;; for the contoller. (PosInt)
    (field [center-x 75])

    ;; the center for calculating distnace (a reference point y co ordinate)
    ;; for the contoller. (PosInt)
    (field [center-y 50])
    
    ;; controller's cache of the position and velocity of the
    ;; particle.
    ;; both Reals.
    (field [x-particle x])
    (field [y-particle y])
    (field [vx-particle 0])
    (field [vy-particle 0])
    
    
    ;; fields for dragging
    ;; box selected refers to the status of the selection of controller by handle.
    (field [box-selected? false])

    ;; controllers chache for last mouse positions.
    (field [saved-mx 0])
    (field [saved-my 0])
    
    ;; box selected refers to the status of the selection of ball for drag.
    (field [for-drag? false])
    
    ;; fields for particle drag
    (field [saved-particle-mx 0])
    (field [saved-particle-my 0])
    
    (super-new)
    
    ;; at initialization, register this controller with the model
    (send model register this)
    
    
    
    ;; distance-x : -> Int
    ;; RETURNS: the distance between (center-x and x-particle)
    (define (distance-x)
      (- x-particle center-x))

    ;; distance-y : -> Int
    ;; RETURNS: the distance between (center-y and y-particle)
    (define (distance-y)
      (- y-particle center-y))
    
    
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position-x? sig)
         (set! x-particle (report-position-x-pos sig))]
        [(report-velocity-x? sig)
         (set! vx-particle (report-velocity-x-v sig))]
        
        [(report-position-y? sig)
         (set! y-particle (report-position-y-pos sig))]
        [(report-velocity-y? sig)
         (set! vy-particle (report-velocity-y-v sig))]))
    
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected for the particle drag or
    ; the handle selected for the controller drag and sends a command to model
    ; to pause the bounce of the particle.
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (cond
        [(in-handle? mx my)
         (begin
           (set! box-selected? true)
           (set! saved-mx (- mx x))
           (set! saved-my (- my y)))]
        [(in-this? mx my)(begin 
                           (set! for-drag? true)
                           (set! saved-particle-mx (- mx x-particle))
                           (set! saved-particle-my (- my y-particle))
                           (send model execute-command (make-pause-the-bounce true)))]))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: location of a button-up event
    ; EFFECT: unselected particle and box both.
    (define/public (after-button-up mx my)
      (begin
        (set! for-drag? false)
        (set! box-selected? false)
        (send model execute-command (make-unpause-the-bounce true))
        ))
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: Updates the saved-mx and saved-my and updates x position in model.
    (define/public (after-drag mx my)
      (cond
        [box-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [for-drag? (begin
                     (send model execute-command (make-set-position-x (- mx saved-particle-mx)))
                     (send model execute-command (make-set-position-y (- my saved-particle-my))))]))
    
    ;; the velocity controller doesn't respond to mouse move events
    (define/public (after-move mx my)
      'velocity-controller-after-move-value)
    
     ; in-handle? Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the handle.
    (define (in-handle? other-x other-y)
      (and
       (and (> other-x (- 10 (- x half-width))) (< other-x (- x half-width)))
       (and (> other-y (- y half-height)) (< other-y (+ 10 (- y half-height))))))
    
    ; in-this?: Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the controller.
    (define (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ x half-width))
       (<= (- y half-height) other-y (+ y half-height))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    
    (define/public (add-to-scene scene)
      (let ((scene (place-image (get-box-image) x y scene)))
        (if in-this?
            (place-image BALL (+ x (distance-x)) (+ y (distance-y)) scene)
            (place-image BALL (+ x (distance-x)) (- y (distance-y)) scene))))
    
    ;; the controller doesn't respond to ticks
    (define/public (after-tick)
      'velocity-controller-after-tick-value)
    
    ;; KeyEvent -> Void
    ;; ignores the key events
    (define/public (after-key-event kev)
      this)
    
    
    
    
    ;; get-box-image : -> Image
    ;; RETURNS : the image of the controller box without the ball
    (define (get-box-image)
      (let ((S (rectangle width height "outline" "blue"))
            (B (rectangle (+ 30 width) height "outline" (current-color)))
            (H (rectangle 10 10 "outline" "black")))
        (overlay/align "left" "top" 
                       H (overlay S B))))
    
    
    
    ;; get-box-image : -> String
    ;; RETURNS : the color of the box
    (define (current-color)
      (if box-selected? "red" "black"))

    

    ;; FOR TESTING PURPOSE

    ;; updates the selected? to true
    (define/public (for-test:select)
      (set! box-selected? true))

    ;; updates the selected for-drag? to true
    (define/public (for-test:select-ball)
      (set! for-drag? true))

    ;; to get x field
    (define/public (for-test:get-x)
      x)

    ;; to get y field
    (define/public (for-test:get-y)
      y)
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; for testing, we'll create a model and a controller for it.
;; We'll send some messages to the controller, and see if the 
;; model gets updated appropriately.

(require rackunit)
(require "extras.rkt")
(require "Model.rkt")

(begin-for-test
  
  (let*
      ((m (make-model))
       (c (make-z-controller m)))
    (begin
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-v) 0)
      
      ;; send m a make-incr-velocity command directly, and see if it
      ;; responds. 
      (send m execute-command (make-incr-velocity-x 2))
      (check-equal? (send m for-test:get-v) 2)
      
      ;; now send the controller "+".  It should send m a
      ;; make-incr-velocity command.  See if that causes the model to
      ;; respond properly.
      (send c for-test:select)
      (send c after-key-event "up")
      (check-equal? (send m for-test:get-v) 2)

      (send c after-button-down 85 80)
      (check-equal? (send m for-test:get-x) 75)
      (send c add-to-scene (empty-scene 600 500))

      (send c after-button-down 100 100)
      (send c add-to-scene (empty-scene 600 500))
      
      ;; does m respond to an after-tick message (yes).
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 75)

      (send c after-key-event "up")
      (check-equal? (send m for-test:get-x) 75)

      (send c after-key-event "down")
      (check-equal? (send m for-test:get-x) 75)

      (send c after-key-event "left")
      (check-equal? (send m for-test:get-x) 75)

      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 75)

      (send c add-to-scene (empty-scene 600 500))

      (send c for-test:select)
      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 75)

      (send c add-to-scene (empty-scene 600 500))

      (send c after-button-up 100 200)
      (send c for-test:select-ball)
      (send c after-drag 85 80)
      (check-equal? (send m for-test:get-x) 60)

      (send c after-button-up 100 200)
      (send c for-test:select)
      (send c after-drag 85 80)
      (check-equal? (send m for-test:get-x) 60)
      
      (send c after-button-down 100 200)
      (check-equal? (send m for-test:get-x) 60)

      (send c after-button-down (send c for-test:get-x) (send c for-test:get-y))
      (send c add-to-scene (empty-scene 600 500))
      (check-equal? (send m for-test:get-x) 60)

      (send c after-button-up 100 200)
      (check-equal? (send m for-test:get-x) 60)

      

      
      
      )))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

