#lang racket

(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "Xcontroller.rkt")
(require "Ycontroller.rkt")
(require "Zcontroller.rkt")

(require 2htdp/universe)

(provide make-controller-factory)

;; make-controller-factory : Container Model -> SWidget
(define (make-controller-factory c m)
  (new ControllerFactory% [c c][m m]))

;; Contstructor Template for ControllerFactory%:
;;  (new ControllerFactory% [c c][m m]))

(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the container in which the controllers will live
    (init-field c)   ; Container

    ; the model to which the controllers will be connected
    (init-field m)   ; Model

    (super-new)

    ; KeyEvent -> Void
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer make-velocity-controller)]
        [(key=? kev "p") (add-viewer make-position-controller)]
        [(key=? kev "x") (add-viewer make-x-controller)]
        [(key=? kev "y") (add-viewer make-y-controller)]
        [(key=? kev "z") (add-viewer make-z-controller)]
        ))


      ;; (Model -> Controller) -> Void
    (define/public (add-viewer make-viewer)
      (send c add-stateful-widget (make-viewer m)))

    ;; the factory is invisible
    (define/public (add-to-scene s) s)

    ;; the factory doesn't respond to any other events

    ;; :-> Void
    (define/public (after-tick) 'controller-factory-after-tick-trap)

    ;; : Int Int -> Void
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)

    ;; : Int Int -> Void
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)

    ;; : Int Int -> Void
    (define/public (after-move mx my)
      'controller-factory-after-move-trap)


    ;; : Int Int -> Void
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)

    ))



