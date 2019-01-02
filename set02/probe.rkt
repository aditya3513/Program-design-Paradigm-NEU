;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")
;; probe.rkt

(provide probe-at
         probe-turned-right
         probe-turned-left
         probe-direction-equal?
         probe-location-equal?
         probe-forward-possible-outcome?
         )
;; extra functions :

(define (angle-left-adjust angle)
  (modulo (+ angle 90) 360)
  )

(define (angle-right-adjust angle)
  (modulo (abs (- angle 90)) 360)
  )

(define (move-east p n)
  (make-probe (- (probe-x p) n) (probe-y p) 0)
  )
(define (move-west p n)
  (make-probe (+ (probe-x p) n) (probe-y p) 180)
  )
(define (move-north p n)
  (make-probe (probe-x p) (- (probe-y p) n) 90)
  )
(define (move-south p n)
  (make-probe (probe-x p) (+ (probe-y p) n) 270)
  )
(define (match-probe-state p1 p2)
  (and (probe-location-equal? p1 p2) (probe-direction-equal? p1 p2))
  )

;;; DATA DEFINITION

(define-struct probe (x y direction))

;; A Probe is a 
;;  (make-probe Int Int int)
;; Interpretation:
;;   x is the x co-ordinate.
;;   y is the y co-ordinate.
;;   direction is angle from 0 degrees i.e East (values possible 0,90,180,270).

;; probe-fn : Probe -> ??
#|                   
(define (probe-fn p)
  (...
    (probe-x p)
    (probe-y p)
    (probe-direction )))
|#


;; probe-at: Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; RETURNS: a probe with its center at those coordinates, facing north.
;; EXAMPLES :
;; (probe-at 1 -2) = (make-probe 1 -2 90)
;; (probe-at 0 0) = (make-probe 0 0 90)
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-at x y)
  (make-probe x y 90)
  )
;; TESTS
(begin-for-test
  (check-equal? (probe-at 1 -2) (make-probe 1 -2 90)
                "It should return (make-probe 1 -2 90)")
  (check-equal? (probe-at 0 0) (make-probe 0 0 90)
                "It should return (make-probe 0 0 90)")
  )

;; dummy probes
(define dummy-probe1 (make-probe 1 2 90))
(define dummy-probe2 (make-probe 0 0 180))
(define dummy-probe3 (make-probe 1 2 90))
(define dummy-probe4 (make-probe 1 3 90))
(define dummy-probe5 (make-probe 1 5 270))

;; probe-turned-left : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees left.
;; EXAMPLES :
;; (probe-turned-left dummy-probe1) = (make-probe 1 2 180)
;; (probe-turned-left dummy-probe2) = (make-probe 0 0 270)
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-turned-left p)
  (make-probe (probe-x p) (probe-y p) (angle-left-adjust (probe-direction p))
              ))
;; TESTS


(begin-for-test
  
  (check-equal? (probe-turned-left dummy-probe1) (make-probe 1 2 180)
                "It should return (make-probe 1 2 180")
  (check-equal? (probe-turned-left dummy-probe2) (make-probe 0 0 270)
                "It should return (make-probe 0 0 270")
  
  ) 

;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees right.
;; EXAMPLES :
;; (probe-turned-right dummy-probe1) = (make-probe 1 2 0)
;; (probe-turned-right dummy-probe2) = (make-probe 0 0 90)
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-turned-right p)
  (make-probe (probe-x p) (probe-y p) (angle-right-adjust (probe-direction p))
              ))

;; TESTS
(begin-for-test
  
  (check-equal? (probe-turned-right dummy-probe1) (make-probe 1 2 0)
                "It should return (make-probe 1 2 0)")
  (check-equal? (probe-turned-right dummy-probe2) (make-probe 0 0 90)
                "It should return (make-probe 0 0 90)")
  )

;; probe-direction-equal? : Probe Probe -> Boolean
;; GIVEN: two probes
;; RETURNS: true iff the two probes are facing in the same direction,
;; else false
;; EXAMPLES :
;; (probe-direction-equal? dummy-probe1 dummy-probe3) = #true
;; (probe-direction-equal? dummy-probe1 dummy-probe2) = #false
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-direction-equal? p1 p2)
  (equal? (probe-direction p1) (probe-direction p2))
  
  )

;; TESTS
(begin-for-test
  
  (check-equal? (probe-direction-equal? dummy-probe1 dummy-probe3) #true
                "It should return true")
  (check-equal? (probe-direction-equal? dummy-probe1 dummy-probe2) false
                "It should return false")
  )

;; probe-location-equal? : Probe Probe -> Boolean
;; GIVEN: two probles
;; RETURNS: true iff the two probes are at the same location
;; EXAMPLES :
;; (probe-location-equal? dummy-probe1 dummy-probe3) = #true
;; (probe-location-equal? dummy-probe1 dummy-probe2) = #false
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-location-equal? p1 p2)
  (and (equal? (probe-x p1) (probe-x p2)) (equal? (probe-y p1) (probe-y p1)))
  )

;; TESTS
(begin-for-test
  
  (check-equal? (probe-location-equal? dummy-probe1 dummy-probe3) #true
                "It should return true")
  (check-equal? (probe-location-equal? dummy-probe1 dummy-probe2) false
                "It should return false")
  )

;; probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
;; GIVEN: two probes and a distance
;; RETURNS: true iff the first probe, given a move-forward command with
;; the specified number of steps, could wind up in the state described by
;; the second probe.
;; EXAMPLES :
;; (probe-forward-possible-outcome? dummy-probe1 1 dummy-probe4) = #true
;; (probe-forward-possible-outcome? dummy-probe1 n dummy-probe5) = #false
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-forward-possible-outcome? p1 n p2)
  (cond
    [(equal? (probe-direction p1) 90)
     (or (or (match-probe-state (move-north p1 (+ n 1)) p2) (match-probe-state (move-north p1 n) p2)) (match-probe-state (move-north p1 (- n 1)) p2))]
    [(equal? (probe-direction p1) 270)
     (or (or (match-probe-state (move-south p1 (+ n 1)) p2) (match-probe-state (move-south p1 n) p2)) (match-probe-state (move-south p1 (- n 1)) p2))]
    [(equal? (probe-direction p1) 0)
     (or (or (match-probe-state (move-east p1 (+ n 1)) p2) (match-probe-state (move-east p1 n) p2)) (match-probe-state (move-east p1 (- n 1)) p2)) ]
    [(equal? (probe-direction p1) 180)
    (or (or (match-probe-state (move-west p1 (+ n 1)) p2) (match-probe-state (move-west p1 n) p2)) (match-probe-state (move-west p1 (- n 1)) p2)) ]
    
    ))

;; TESTS
(check-equal? (probe-forward-possible-outcome? dummy-probe1 1 dummy-probe4) true
              "It should return true")
(check-equal? (probe-forward-possible-outcome? dummy-probe1 5 dummy-probe5) false
              "It should return false")  





