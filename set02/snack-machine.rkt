;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
;; snack-machine.rkt

(provide
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-kale
 machine-remaining-carrots
 machine-bank
 ) 


;; DATA DEFINITION:

;; A CustomerInput is one of
;; -- a PosInt        interp: insert the specified number of quarters
;; -- "kale"          interp: request a bag of kale chips
;; -- "carrots"       interp: request a bag of carrots
;; -- "change"        interp: return all the unspent money that the
;;                            customer has inserted

;; A MachineOutput is one of
;; -- "kale"           interp: machine dispenses a bag of kale chips
;; -- "carrots"        interp: machine dispenses a bag of carrot sticks
;; -- "Out of Item"    interp: machine displays "Out of Item"
;; -- a PosInt         interp: machine releases the specified number of quarters
;; -- "Nothing"        interp: the machine does nothing


(define-struct machinestate (kc cs bal bank))

;; A MachineState is a 
;;  (make-machinestate PosInt PosInt PosInt PosInt)
;; Interpretation:
;;   kc is the number of kale chips.
;;   cs is the number of carrot sticks.
;;   bal is the balance that is to be returned to user.
;;   bank is the deposit where amount is saved after successfull transaction.


;; TEMPLATE
;; machinestate-fn : MachineState -> ??
#|
(define (machinestate-fn kale-chips carrot-sticks balance bank)
 
  (...
    (machinestate-ks x)
    (machinestate-cs x)
    (machinestate-bal x)
    (machinestate-bank x)))  
|#


;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of bags of kale chips and carrot sticks
;; RETURNS: the state of a machine loaded with the given numbers of bags
;; of kale chips and carrot sticks, with an empty bank.
;; EXAMPLES:
;; (initial-machine 1 2 ) = (make-machinestate 1 2 0 0)
;; (initial-machine 0 5 ) = (make-machinestate 0 5 0 0)
;; STRATEGY: Use template for machinestate 

(define (initial-machine k c)
  
  (make-machinestate k c 0 0)
  )

;;TESTS
(begin-for-test
  (check-equal? (initial-machine 1 2 ) (make-machinestate 1 2 0 0) "it should return (make-machinestate 1 2 0 0)")
  
  (check-equal? (initial-machine 0 5 ) (make-machinestate 0 5 0 0) "it should return (make-machinestate 0 5 0 0)"))


;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's input
;; EXAMPLES:
;; (machine-next-state dummy1 5 ) = (make-machinestate 1 2 5 0)
;; (machine-next-state (make-machinestate 1 2 5 0) "change" ) = (make-machinestate 1 2 0 0)
;; STRATEGY: Use template for machinestate 

(define (machine-next-state ms ci)
  (
   if (number? ci)
      (make-machinestate (machinestate-kc ms) (machinestate-cs ms) (+ (machinestate-bal ms) ci) (machinestate-bank ms))
      (cond
        [(equal? ci "kale")
         (if (and (>= (machinestate-bal ms) 2) (> (machinestate-kc ms) 0)) (make-machinestate (- (machinestate-kc ms) 1) (machinestate-cs ms) (- (machinestate-bal ms) 2) (+ (machinestate-bank ms) 0.5))
             ms)]
        [(equal? ci "carrot")
         (if (and (>= (machinestate-bal ms) 3) (> (machinestate-cs ms) 0)) (make-machinestate (machinestate-kc ms) (- (machinestate-cs ms) 1) (- (machinestate-bal ms) 3) (+ (machinestate-bank ms) 0.75))
             ms)]
        [(equal? ci "change")
         (if (> (machinestate-bal ms) 0) (make-machinestate (machinestate-kc ms) (machinestate-cs ms) 0 (machinestate-bank ms))
             ms)]
        )
      )
  )
;; TESTS:
(define dummy1 (make-machinestate 1 2 0 0))
(begin-for-test 
  (check-equal? (machine-next-state dummy1 "kale" ) dummy1 "it should return dummy1")
  (check-equal? (machine-next-state dummy1 5 ) (make-machinestate 1 2 5 0) "it should return (make-machinestate 1 2 5 0)")
  (check-equal? (machine-next-state (make-machinestate 1 2 5 0) "change" ) (make-machinestate 1 2 0 0) "it should return (make-machinestate 1 2 0 0)")
  
  )

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input
;; EXAMPLES:
;; (machine-output dummy1 "kale" ) = "nothing"
;; (machine-output (make-machinestate 1 2 5 0) "change" ) = 5
;; STRATEGY: Use template for machinestate 

(define (machine-output ms ci)
  (
   if (number? ci)
      "nothing"
      (cond
        [(equal? ci "kale")
         (if (and (>= (* (machinestate-bal ms) 0.25) 0.5) (> (machinestate-kc ms) 0)) "kale"
             "nothing")]
        [(equal? ci "carrot")
         (if (and (>= (* (machinestate-bal ms) 0.25) 0.75) (> (machinestate-cs ms) 0)) "carrot"
             "nothing")]
        [(equal? ci "change")
         (if (> (* (machinestate-bal ms) 0.25) 0) (machinestate-bal ms)
             0)]
        )
      )
  )
;; TESTS:

(begin-for-test 
  
  (check-equal? (machine-output dummy1 "kale" ) "nothing" "it should return nothing")
  (check-equal? (machine-output (make-machinestate 1 2 5 0) "change" ) 5 "it should return 5")
  
  )

;; machine-remaining-kale : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of kale chips left in the machine
;; EXAMPLES:
;; (machine-remaining-kale dummy1 ) = 1
;; (machine-next-state (make-machinestate 1 2 5 0) "kale") ) = 0
;; STRATEGY: Use template for machinestate

(define (machine-remaining-kale ms)
  (machinestate-kc ms))
;; TESTS:
(begin-for-test 
  
  (check-equal? (machine-remaining-kale dummy1 ) 1 "it should return 1")
  (check-equal? (machine-remaining-kale (machine-next-state (make-machinestate 1 2 5 0) "kale") ) 0 "it should return 0")
  
  )


;; machine-remaining-carrots : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of carrots left in the machine
;; EXAMPLES:
;; (machine-remaining-carrots dummy1 ) = 2
;; (machine-remaining-carrots (machine-next-state (make-machinestate 1 2 5 0) "carrot") ) = 1
;; STRATEGY: Use template for machinestate

(define (machine-remaining-carrots ms)
  (machinestate-cs ms))
;; TESTS:
(begin-for-test 
  
  (check-equal? (machine-remaining-carrots dummy1 ) 2 "it should return 2")
  (check-equal? (machine-remaining-carrots (machine-next-state (make-machinestate 1 2 5 0) "carrot") ) 1 "it should return 1")
  
  )

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLES:
;; (machine-bank (make-machinestate 1 2 5 0) ) = 0
;; (machine-bank (machine-next-state (make-machinestate 1 2 3 0) "carrot") ) = 0.75
;; STRATEGY: Use template for machinestate

(define (machine-bank ms)
  (machinestate-bank ms))
;; TESTS:
(begin-for-test 
  
  (check-equal? (machine-bank (make-machinestate 1 2 5 0) ) 0 "it should return 0")
  (check-equal? (machine-bank (machine-next-state (make-machinestate 1 2 3 0) "carrot") ) 0.75 "it should return 0.75")
  
  )

