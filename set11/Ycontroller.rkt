#lang racket


(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require "ZController.rkt")

(provide make-y-controller)


(define RED-CIRCLE (circle 10 "solid" "red"))
(define BLACK-DOT (circle 2 "solid" "black"))
(define BALL (overlay/align "middle" "middle" BLACK-DOT RED-CIRCLE))

;; make-y-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A y controller for m

(define (make-y-controller m)
  (new YController% [model m]))


;; a YController% is a (new YController% [model Model<%>])
;; inherits XYController%

(define YController%
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
    ;; both Reals.
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
      (- y-particle center-y))

    
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/override (receive-signal sig)
      (cond
        [(report-position-y? sig)
         (set! y-particle (report-position-y-pos sig))]
        [(report-velocity-y? sig)
         (set! vy-particle (report-velocity-y-v sig))]
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

    ;; the velocity controller doesn't respond to mouse move events
    (define/override (after-move mx my)
      'velocity-controller-after-move-value)
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; EFFECT: Updates the saved-mx and saved-my and updates y position in model.
    (define/override (after-drag mx my)
      (cond
        [box-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [for-drag? (begin
                     (send model execute-command (make-set-position-y (- my saved-particle-my))))]))
    
    
    ; in-handle? Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the handle.
    (define (in-handle? other-x other-y)
      (and
       (and (> other-x (- 10 (- x (/ half-width 2)))) (< other-x (- x (/ half-width 2))))
       (and (> other-y (- y half-height)) (< other-y (+ 10 (- y half-height))))))
    
    ; in-this?: Integer Integer -> Boolean
    ; GIVEN: The mouse coordinates
    ; RETURNS : True iff the mouse is inside the controller.
    (define (in-this? other-x other-y)
      (and
       (<= (- x (/ half-width 2)) other-x (+ x (/ half-width 2)))
       (<= (- y half-height) other-y (+ y half-height))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this controller painted
    ;; on it.
    ;; STRATEGY: place the image of controller centered at x y
    (define/override (add-to-scene scene)
      (let ((scene (place-image (get-box-image) x y scene)))
        (if in-this?
            (place-image BALL x (+ y (distance)) scene)
            (place-image BALL x (- y (distance)) scene))))
    
    
 
   

    ;; get-box-image : -> Image
    ;; RETURNS : the image of the controller box without the ball
    (define (get-box-image)
      (let ((S (rectangle (/ width 2) height "outline" "blue"))
        (B (rectangle (+ 30 (/ width 2)) height "outline" (current-color)))
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
       (c (make-y-controller m)))
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
      (check-equal? (send m for-test:get-x) 75)

      (send c after-button-up 100 200)
      (send c for-test:select)
      (send c after-drag 85 80)
      (check-equal? (send m for-test:get-x) 75)
      
      (send c after-button-down 100 200)
      (check-equal? (send m for-test:get-x) 75)

      (send c after-button-down (send c for-test:get-x) (send c for-test:get-y))
      (send c add-to-scene (empty-scene 600 500))
      (check-equal? (send m for-test:get-x) 75)

      (send c after-button-up 100 200)
      (check-equal? (send m for-test:get-x) 75)

      

      
      
      )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

