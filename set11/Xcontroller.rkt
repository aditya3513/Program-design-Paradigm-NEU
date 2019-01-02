#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require "Zcontroller.rkt")

(provide make-x-controller)


(define RED-CIRCLE (circle 10 "solid" "red"))
(define BLACK-DOT (circle 2 "solid" "black"))
(define BALL (overlay/align "middle" "middle" BLACK-DOT RED-CIRCLE))

;; make-x-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A x controller for m

(define (make-x-controller m)
  (new XController% [model m]))


;; a XController% is a (new XController% [model Model<%>])
;; inherits XYController%

(define XController%
  (class* ZController% (Controller<%>)
    
    ;; the model
    (inherit-field model)
 
    
    ;; center coordinates of the controller
    ; both of these are NonNegInts.
    (inherit-field x y)   
    
    ;; canvas boundary in which the particle bounces.  Both PosInts.
    (inherit-field width height)

    ;; half the width of the contoller is saved here. (PosInt)
    (inherit-field half-width)

    ;; half the height of the contoller is saved here. (PosInt)
    (inherit-field half-height)

    ;; the center for calculating distnace (a reference point x co ordinate)
    ;; for the contoller. (PosInt)
    (inherit-field center-x)

    ;; the center for calculating distnace (a reference point y co ordinate)
    ;; for the contoller. (PosInt)
    (inherit-field center-y)
    
    ;; controller's cache of the position and velocity of the
    ;; particle.
    (inherit-field x-particle)
    (inherit-field y-particle)
    (inherit-field vx-particle)
    (inherit-field vy-particle)
    
    
    ;; fields for dragging
    ;; box selected refers to the status of the selection of controller by handle.
    (inherit-field box-selected?)

    ;; box selected refers to the status of the selection of ball for drag.
    (inherit-field for-drag?)

    ;; controllers chache for last mouse positions.
    (inherit-field saved-mx)
    (inherit-field saved-my)

    ;; controllers chache for last particle positions.
    (inherit-field saved-particle-mx)
    (inherit-field saved-particle-my)
    
    (super-new)
    
    ;; at initialization, register this controller with the model
    (send model register this)

    ;; distance : -> Int
    ;; RETURNS: the distance between (center-x and x-particle)
    (define (distance)
      (- x-particle center-x))

    
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/override (receive-signal sig)
      (cond
        [(report-position-x? sig)
         (set! x-particle (report-position-x-pos sig))]
        [(report-velocity-x? sig)
         (set! vx-particle (report-velocity-x-v sig))]
        [else this]))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected for the particle drag or
    ; the handle selected for the controller drag and sends a command to model
    ; to pause the bounce of the particle.
    ; STRATEGY: Cases on whether the event is in this object
    (define/override (after-button-down mx my)
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
    (define/override (after-button-up mx my)
      (begin
        (set! for-drag? false)
        (set! box-selected? false)
        (send model execute-command (make-unpause-the-bounce true))
        ))
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: Updates the saved-mx and saved-my and updates x position in model.
    (define/override (after-drag mx my)
      (cond
        [box-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [for-drag? (begin
                     (send model execute-command (make-set-position-x (- mx saved-particle-mx))))]))
    

    ;; the velocity controller doesn't respond to mouse move events
    (define/override (after-move mx my)
      'velocity-controller-after-move-value)
    
    ; in-handle? Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the handle.
    (define (in-handle? other-x other-y)
      (and
       (and (> other-x (- 10 (- x half-width))) (< other-x (- x half-width)))
       (and (> other-y (- y (/ half-height 2))) (< other-y (+ 10 (- y (/ half-height 2)))))))
    
    ; in-this?: Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the controller.
    (define (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ x half-width))
       (<= (- y (/ half-height 2)) other-y (+ y (/ half-height 2)))))

    
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this controller painted
    ;; on it.
    ;; STRATEGY: place the image of controller centered at x y
    (define/override (add-to-scene scene)
      (let ((scene (place-image (get-box-image) x y scene)))
            (place-image BALL (+ x (distance)) y scene)))
    
  
    ;; get-box-image : -> Image
    ;; RETURNS : the image of the controller box without the ball
    (define (get-box-image)
      (let ((S (rectangle width (/ height 2) "outline" "blue"))
        (B (rectangle (+ 30 width) (/ height 2) "outline" (current-color)))
        (H (rectangle 10 10 "outline" "black")))
        (overlay/align "left" "top" 
         H (overlay S B))))

    
    
    ;; get-box-image : -> String
    ;; RETURNS : the color of the box
    (define (current-color)
      (if box-selected? "red" "black"))
    

    
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
       (c (make-x-controller m)))
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




