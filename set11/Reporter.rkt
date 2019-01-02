#lang racket

(require "interfaces.rkt")
(require "WidgetWorks.rkt")
(require "Model.rkt")
(require "ControllerFactory.rkt")

;; run with (run 0.5)

;; create a container, install a factory, and run.

(define (try rate)
  (let ((c (container-init 600 500))
        (m (make-model)))
    (begin
      (send c add-stateful-widget m)
      (send c add-stateful-widget (make-controller-factory c m))
      (send c run rate))))



(provide make-reporter)

;; Model -> Reporter
(define (make-reporter m)
  (new Reporter% [model m]))

;; set up a little object that just receives signals from a model.

(define Reporter%
  (class* object% ()  ;; too lazy to write a Reporter<%> interface
    (init-field model)
    ;; the position and velocity reported by the model
    (field [x 0][v 0])
    (super-new)
    (send model register this)

   ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! x (report-position-pos sig))]
        [(report-velocity? sig)
         (set! v (report-velocity-v sig))]))

    (define/public (model-x) x)
    (define/public (model-v) v)

    (define/public (get-x) x)
    (define/public (get-v) v)

    (define/public (model-state)
      (list 'Position: x 'Velocity: v))

    ))


(define c (container-init 600 500))
(define m (make-model))
(send c add-stateful-widget m)
(send c add-stateful-widget (make-controller-factory c m))

(define r (make-reporter m))

;;(send m publish-position)

(send r get-x)