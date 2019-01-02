#lang racket

;; interfaces for MVC example

(require "WidgetWorks.rkt")

(provide Controller<%> Model<%>)

;; structs for model command language
(provide 
  (struct-out set-position-x) 
  (struct-out incr-velocity-x)
  (struct-out report-position-x)
  (struct-out report-velocity-x)
  (struct-out set-position-y) 
  (struct-out incr-velocity-y)
  (struct-out report-position-y)
  (struct-out report-velocity-y)
  (struct-out pause-the-bounce)
  (struct-out unpause-the-bounce))

;; A Controller is an object of any class that implements
;; Controller<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal
    
    ))

;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the Container, so they must implement the
;; SWidget<%> interface in order to do so.

(define Model<%>
  (interface (SWidget<%>)

    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; CONTROLLER/MODEL PROTOCOL:

;; As soon as a controller registers with the model, the model sends
;; the controller a pair of Signals so the controller will know the
;; current state of the model.

;; The controller then sends the model commands, which the model is
;; supposed to execute.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position n)     
;; -- (make-incr-velocity dv)

;; A Signal is one of
;; -- (make-report-position n)
;; -- (make-report-velocity v)

;; n, v, dv are all Reals.

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.


(define-struct set-position-x (pos) #:transparent)
(define-struct incr-velocity-x (dv) #:transparent)
(define-struct report-position-x (pos) #:transparent)
(define-struct report-velocity-x (v) #:transparent)

(define-struct set-position-y (pos) #:transparent)
(define-struct incr-velocity-y (dv) #:transparent)
(define-struct report-position-y (pos) #:transparent)
(define-struct report-velocity-y (v) #:transparent)

(define-struct pause-the-bounce (paused) #:transparent)
(define-struct unpause-the-bounce (unpaused) #:transparent)










