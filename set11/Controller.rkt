#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; +,- increments or decrements the speed of the particle

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")

(provide Controller%)





;; Constructor template for Controller%
;; (new Controller% [model Model])

(define Controller%
  (class* object% (Controller<%>)
    
    (init-field model)  ; the model
    
    ; Position of the center of the controller
    ; both of these are NonNegInts.
    (init-field [x 150] [y 100])   
    
    ; width and height of the controller.  Both PosInts.
    (init-field [width 120][height 50])

     ; half the width and half the height of the controller.  Both PosInts.
    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])
    
    ;; controller's cache of the position and velocity of the
    ;; particle.
    ;; both Reals.
    (field [particle-x 0])
    (field [particle-vx 0])
    (field [particle-y 0])
    (field [particle-vy 0])
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])
    
    (super-new)
    
    ;; at initialization, register this controller with the model
    (send model register this)
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position-x? sig)
         (set! particle-x (report-position-x-pos sig))]
        [(report-velocity-x? sig)
         (set! particle-vx (report-velocity-x-v sig))]

        [(report-position-y? sig)
         (set! particle-y (report-position-y-pos sig))]
        [(report-velocity-y? sig)
         (set! particle-vy (report-velocity-y-v sig))]))
    
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; The viewer stays selected until a button down somewhere else
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          (set! selected? false)))
    
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: ignores button down
    (define/public (after-button-up mx my) 'ignored)

    
    
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered controllers.
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          'velocity-controller-after-drag-value))
    
    ;; the velocity controller doesn't respond to mouse move events
    (define/public (after-move mx my)
      'velocity-controller-after-move-value)
    
    ;; Int Int -> Boolean
    (define (in-this? other-x other-y)
      (and
       (and (> other-x (- 10 (- x half-width))) (< other-x (- x half-width)))
       (and (> other-y (- y half-height)) (< other-y (+ 10 (- y half-height))))))
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this controller painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y on given scene
    
    (define/public (add-to-scene scene)
      (place-image (viewer-image-with-handle) x y scene))
    
    ;; the controller doesn't respond to ticks
    (define/public (after-tick)
      'velocity-controller-after-tick-value)
    
    ;; KeyEvent -> Void
    ;; interpret +,- as commands to the model
    ;; +/- alter velocity of the particle
    (define/public (after-key-event kev)
      (if selected?
          (cond
            [(key=? "right" kev)
             (send model execute-command (make-incr-velocity-x 1))]
            [(key=? "left" kev)
             (send model execute-command (make-incr-velocity-x -1))]
            
            [(key=? "up" kev)
             (send model execute-command (make-incr-velocity-y 1))]
            [(key=? "down" kev)
             (send model execute-command (make-incr-velocity-y -1))])
          3456))
    
    ;; -> Image
    ;; RETURNS: the image of the viewer
    ;; STRATEGY: assemble the image from the data and rectangle
    (define (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
         the-data-image
         (rectangle
          (max width (+ (image-width the-data-image) 10))
          (max height (+ (image-height the-data-image) 10))
          "outline" 
          (current-color)))))
    
    
    ;; -> Image
    ;; RETURNS: the image of the viewer
    ;; STRATEGY: assemble the image from the data and rectangle
    (define (viewer-image-with-handle)
      (let ((the-data-image (viewer-image)))
        (overlay/align "left" "top" (rectangle 10 10 "outline" "black") the-data-image)))
    
    
    
    ;; -> Image
    (define (data-image)
      (above
       (text "ARROWS : Change velocity" 10 "black")
       (text (string-append (text-x)
             (text-y))
             12
             "black")))

    ;; :-> String
    ;; RETURNS : the values of x and vx for placing on the conroller image 
    (define (text-x)
      (string-append
              "X = "
              (number->string particle-x)
              " VX = "
              (number->string particle-vy)))

    ;; :-> String
    ;; RETURNS : the values of y and vy for placing on the conroller image 
    (define (text-y)
      (string-append
              "Y = "
              (number->string particle-y)
              " VY = "
              (number->string particle-vx)))

    ;; :-> String
    ;; RETURNS : the curent color of controller box 
    (define (current-color)
      (if selected? "red" "black"))


    ;; FOR TEST
    (define/public (for-test:select)
      (set! selected? true))


    
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
       (c (new Controller% [model m])))
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
      
      ;; does m respond to an after-tick message (yes).
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 77)

      (send c after-key-event "up")
      (check-equal? (send m for-test:get-x) 77)

      (send c after-key-event "down")
      (check-equal? (send m for-test:get-x) 77)

      (send c after-key-event "left")
      (check-equal? (send m for-test:get-x) 77)

      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 77)

      (send c add-to-scene (empty-scene 600 500))

      (send c for-test:select)
      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 77)

      (send c add-to-scene (empty-scene 600 500))

      (send c after-drag 10 10)
      (check-equal? (send m for-test:get-x) 77)
      
      (send c after-button-down 100 200)
      (check-equal? (send m for-test:get-x) 77)

      
      
      )))



