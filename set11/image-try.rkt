#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "PerfectBounce.rkt")

(define RED-CIRCLE (circle 10 "solid" "red"))
(define black-dot (circle 2 "solid" "black"))
(define BALL (overlay black-dot RED-CIRCLE))



(define CW 100)
(define CH 100)
(define EMPTY-CANVAS (empty-scene CW CH))

;; sum-square : PosInt PosInt -> PosInt
    ;; GIVEN: values of x and y
    ;; RETURNS: the square of the difference between x and y
    ;; EXAMPLES: (sum-square 3 5) => 4
    ;; STRATEGY: Combine simpler functions
    (define (sum-square z1 z2)
      (sqr (- z1 z2)))

    ;; distance : PosInt PosInt PosInt PosInt -> PosReal
    ;; GIVEN: two points in the plane
    ;; RETURNS: the distance between the points
    ;; EXAMPLES: (distance 4 3 7 7) => 5
    ;; STRATEGY: Combine simpler functions
    (define (distance x1 y1 x2 y2)
      (sqrt (+ (sum-square x1 x2) (sum-square y1 y2))))

(distance 1 10 0 0)

