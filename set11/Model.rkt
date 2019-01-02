#lang racket

;; The model consists of the a particle boucing in different dimensions based
;; on the position and the velocity. It accepts the key events "x", "y", "z" which
;; display XController, YController and ZController respectively on the scene.
;; Particle moves only in the x direction in the XController, y in the YController
;; and in both x and y directions in XY Controller.

;; It accepts the commands from velocty and position controller which appear on
;; the key press of "v" and "p" respectively. These controllers with the combination
;; of the arrow keys and mouse down control all the positions, x and y, and
;; velocities, vx and vy. Model computes the position and velocities and reports
;; them to the controllers which are registered with model.

;; It also executes the commands that the position and the velocity controller send it.


(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")

(provide make-model)

;; -> Model
(define (make-model) (new Model%))

;; Constructor template for Model%:
;; (new Model%)

(define Model%
  (class* object% (Model<%>)

    ; field to determine of the bounce is paused or not.
    (init-field [paused? false])

    ;; boundaries of the field
    (field [xmin 0])
    (field [xmax 150])
    (field [ymin 0])
    (field [ymax 100])

    ;; rectangle for perfect bounce
    (field [rect (make-rect xmin xmax ymin ymax)])

    ;; position and velocity of the object
    (init-field [x (/ xmax 2)])
    (init-field [y (/ ymax 2)])
    (init-field [vx 0])
    (init-field [vy 0])

    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])

    

    (super-new)

    ;; after-tick: -> Void
    ;; EFFECT: moves the object by v.
    ;; if the resulting x is >= 150 or <= 0, similarly for y
    ;; reports x at ever tick and it
    ;; reports velocity only when it changes during the unpaused state.
    ;; Does nothing when the bounce is paused.
    ;; Speed and velocity of the particle are calculated in the
    ;; particle-after-tick function in the PerfectBounce.rkt file.
    (define/public (after-tick)
      (if paused?
          this
          (local
            ((define p-after-tick (particle-after-tick (make-particle x y vx vy)
                                            (make-rect xmin xmax ymin ymax))))
            (update-position-x (particle-x p-after-tick))
         (update-velocity-x (particle-vx p-after-tick))
       (update-position-y (particle-y p-after-tick))
         (update-velocity-y (particle-vy p-after-tick))
          )))


    

    ;; Controller -> Void
    ;; register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position-x x))
        (send c receive-signal (make-report-velocity-x vx))
        (send c receive-signal (make-report-velocity-x vx))
        (send c receive-signal (make-report-velocity-y vy))))

    ;; Command -> Void
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position-x? cmd)
         (begin
           (cond
             [(>= (set-position-x-pos cmd) xmax) (set! x (- xmax 1))]
             [(<= (set-position-x-pos cmd) xmin) (set! x (+ xmin 1))]
             [else (set! x (set-position-x-pos cmd))])
           (publish-position-x))]
        [(incr-velocity-x? cmd)
         (begin
           (cond
             [(>= x xmax) (set! vx (+ vx (* -1 (incr-velocity-x-dv cmd))))]
             [(<= x xmin) (set! vx (+ vx (* -1 (incr-velocity-x-dv cmd))))]
             [else (set! vx (+ vx (incr-velocity-x-dv cmd)))])
           
           (publish-velocity-x))]
;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(set-position-y? cmd)
         (begin
           (cond
             [(>= (set-position-y-pos cmd) ymax) (set! y (- ymax 1))]
             [(<= (set-position-y-pos cmd) ymin) (set! y (+ ymin 1))]
             [else (set! y (set-position-y-pos cmd))])
           (publish-position-y))]
        [(incr-velocity-y? cmd)
         (begin
           (cond
             [(>= y ymax) (set! vy (+ vy (* -1 (incr-velocity-y-dv cmd))))]
             [(<= y ymin) (set! vy (+ vy (* -1 (incr-velocity-y-dv cmd))))]
             [else (set! vy (+ vy (incr-velocity-y-dv cmd)))])
           
           (publish-velocity-y))]

        [(pause-the-bounce? cmd)
         (set! paused? true)]
        [(unpause-the-bounce? cmd)
         (set! paused? false)]))



    ;; publis-position-x: -> Void
    ;; EFFECT: reports x position to each controller:
    (define (publish-position-x)
      (let ((msg (make-report-position-x x)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    ;; publis-position-vx: -> Void
    ;; EFFECT: reports vx position to each controller:
    (define (publish-velocity-x)
      (let ((msg (make-report-velocity-x vx)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ;; publis-position-y: -> Void
    ;; EFFECT: reports y position to each controller:
    (define (publish-position-y)
      (let ((msg (make-report-position-y y)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    ;; publis-position-y: -> Void
    ;; EFFECT: reports vy position to each controller:
    (define (publish-velocity-y)
      (let ((msg (make-report-velocity-y vy)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages

    ; : Int Int -> Void
    (define/public (after-button-down mx my) 'trap)

    ; : Int Int -> Void
    (define/public (after-button-up mx my) 'trap)

    ; : Int Int -> Void
    (define/public (after-drag mx my) 'trap)

    ; : Int Int -> Void
    (define/public (after-move mx my) 'trap)

    ; : KeyEvent -> Void
    (define/public (after-key-event kev) 'trap)

    ; : Scene -> Void
    (define/public (add-to-scene s) s)

    
    ; : Int -> Void
    ;; EFFECT : updates vx with new vx value and send this to all controllers
    (define/public (update-velocity-x v-new)
      (begin
        (set! vx v-new)
        (publish-velocity-x)))

    ; : Int Int -> Void
    ;; EFFECT : updates x with new x value and send this to all controllers
    (define/public (update-position-x x-new)
      (begin
        (set! x x-new)
        (publish-position-x)))

    ; : Int Int -> Void
    ;; EFFECT : updates vy with new vy value and send this to all controllers
    (define/public (update-velocity-y v-new)
      (begin
        (set! vy v-new)
        (publish-velocity-y)))

    ; : Int Int -> Void
    ;; EFFECT : updates y with new y value and send this to all controllers
    (define/public (update-position-y y-new)
      (begin
        (set! y y-new)
        (publish-position-y)))

    ;; test methods
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-v) vx)
    
    ))

;;; tests

;; check to see if the model responds to incr-velocity commands (it
;; does) and to after-tick messages

;(begin-for-test
;
;  (let*
;      ((m (make-model)))
;    (begin
;      (check-equal? (send m for-test:get-x) 100)
;      (check-equal? (send m for-test:get-v) 0)
;      (send m after-tick)
;      (check-equal? (send m for-test:get-x) 100)
;      (check-equal? (send m for-test:get-v) 0)
;
;      (send m execute-command (make-incr-velocity 2))
;      (check-equal? (send m for-test:get-v) 2)
;
;      (send m after-tick)
;      (check-equal? (send m for-test:get-x) 102)
;      (check-equal? (send m for-test:get-v) 2)
;
;      (send m after-tick)
;      (check-equal? (send m for-test:get-x) 104)
;      (check-equal? (send m for-test:get-v) 2)
;      
;      )))

;; m is definitely responding to after-tick messages.  Is it not
;; getting the after-tick messages from big-bang?

;; Ans: yes, that's the problem:  In top.rkt, I created the model, but
;; never added it to the Container (duh!)





    
  




    

    
