#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; +,- increments or decrements location of the particle by 5

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "Controller.rkt")

(provide make-position-controller)

;; make-position-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A position controller for m

(define (make-position-controller m)
  (new PositionController% [model m]))


;; Constructor template for PositionController%
;; (new PositionController% [model Model])

(define PositionController%
  (class* Controller% (Controller<%>)
    
    (inherit-field model)  ; the model
    
    ; Position of the center of the controller
    ; both of these are NonNegInts.
    (inherit-field x y)   
    
    ; width and height of the controller.  Both PosInts.
    (inherit-field width height)
    
    (inherit-field half-width)
    (inherit-field half-height)
    
    ;; controller's cache of the position and velocity of the
    ;; particle.
    ;; both Reals.
    (inherit-field particle-x)
    (inherit-field particle-vx)
    (inherit-field particle-y)
    (inherit-field particle-vy)
    
    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer
    (inherit-field selected?)
    (inherit-field saved-mx)
    (inherit-field saved-my)
    
    (super-new)
    
    ;; at initialization, register this controller with the model
    (send model register this)
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/override (receive-signal sig)
      (cond
        [(report-position-x? sig)
         (set! particle-x (report-position-x-pos sig))]
        [(report-velocity-x? sig)
         (set! particle-vx (report-velocity-x-v sig))]

        [(report-position-y? sig)
         (set! particle-y (report-position-y-pos sig))]
        [(report-velocity-y? sig)
         (set! particle-vy (report-velocity-y-v sig))]))
    
    
    
    
    
    ;; KeyEvent -> Void
    ;; interpret arrow keys as commands to the model
    ;; up/down/right/left alter position of the particle
    (define/override (after-key-event kev)
      (if selected?
          (cond
            [(key=? "right" kev)
             (send model execute-command
                   (make-set-position-x (+ particle-x 5)))]
            [(key=? "left" kev)
             (send model execute-command
                   (make-set-position-x (- particle-x 5)))]

             [(key=? "up" kev)
             (send model execute-command
                   (make-set-position-y (+ particle-y 5)))]
            [(key=? "down" kev)
             (send model execute-command
                   (make-set-position-y (- particle-y 5)))])
          'position-controller-after-key-event-value))
    
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

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    
    (define/override (add-to-scene scene)
      (place-image (viewer-image-with-handle) x y scene))
    
    ;; :-> Image
    (define (data-image)
      (above
       (text "ARROWS : Change Position" 10 "black")
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
              (number->string particle-vx)))

    ;; :-> String
    ;; RETURNS : the values of y and vy for placing on the conroller image 
    (define (text-y)
      (string-append
              "Y = "
              (number->string particle-y)
              " VY = "
              (number->string particle-vy)))

    ;; :-> String
    ;; RETURNS : the current color of the conroller box.
    (define (current-color)
      (if selected? "red" "black"))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for testing, we'll create a model and a controller for it.
;; We'll send some messages to the controller, and see if the 
;; model gets updated appropriately.

(require rackunit)
(require "extras.rkt")
(require "Model.rkt")

(begin-for-test
  
  (let*
      ((m (make-model))
       (c (make-position-controller m)))
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
      (check-equal? (send m for-test:get-x) 72)

      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 77)

      (send c add-to-scene (empty-scene 600 500))

      (send c for-test:select)
      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 82)

      (send c add-to-scene (empty-scene 600 500))

      (send c after-drag 10 10)
      (check-equal? (send m for-test:get-x) 82)
      
      (send c after-button-down 100 200)
      (check-equal? (send m for-test:get-x) 82)

      
      
      )))





