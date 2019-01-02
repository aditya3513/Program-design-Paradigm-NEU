;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")
;; fsm.rkt

(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  ) 


;; DATA DEFINITION:
(define-struct state (s))
;; a State s is one of the following Strings
;; -- "S0"
;; -- "S1"
;; -- "S2"
;; -- "S3"
;; -- "S4"

;; INTERPRETATION: refer to table below:
#|
State        Input    Output-State
-----------------------------------
"S0"        "q" or "x"     "S1"
"S0"           "u"         "S2"
"S0"        "a" or "b"     "S3"
"S0"            "d"        "S4"

"S1"        "q" or "x"     "S1"
"S1"           "u"         "S2"
"S1"        "a" or "b"     "S3"
"S1"           "d"         "S4"

"S2"           "u"         "S2"
"S2"        "a" or "b"     "S3"
"S2"           "d"         "S4"

"S3"        "a" or "b"     "S3"
"S3"           "d"         "S4"

"S4"        "e" or "f"     "S4"
-----------------------------
|#

;; a MachineInput is one of the Strings
;; -- "q"
;; -- "x" 
;; -- "u"
;; -- "a"
;; -- "b" 
;; -- "d"
;; -- "e"
;; -- "f" 

;; TEMPLATE
;; state-fn : State -> ??
#|
(define (state-fn s machineinput)
 (cond
   [(string=? s machineinput)  ...]))  
|#

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
;; EXAMPLES:
#| (next-state 1) = "INTERPRETATION: refer to table below:

State        Input    Output-State
-----------------------------------
S0           q or x       S1
S0              u         S2
S0           a or b       S3
S0              d         S4

S0 is the initial state of the finite state machine."
|#

;; STRATEGY: Use template for State on state

(define (initial-state n)
  (if (number? n) (make-state "S0") "please enter number"
                                                      )
  )
;; TESTS


(begin-for-test
  
  (check-equal? (initial-state 1) (make-state "S0")))

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; EXAMPLES:
;; (next-state (make-state "S0") "q") =  "S1"
;; (next-state (make-state "S0") "d") =  "S4"
;; STRATEGY: Use template for State on state

(define (next-state sn machineinput)
  
  (cond
    [(and (equal? (state-s sn) "S0") (equal? machineinput "q")) (make-state "S1")]
    [(and (equal? (state-s sn) "S0") (equal? machineinput "x")) (make-state "S1")]
    [(and (equal? (state-s sn) "S0") (equal? machineinput "u")) (make-state "S2")]
    [(and (equal? (state-s sn) "S0") (equal? machineinput "a")) (make-state "S3")]
    [(and (equal? (state-s sn) "S0") (equal? machineinput "b")) (make-state "S3")]
    [(and (equal? (state-s sn) "S0") (equal? machineinput "d")) (make-state "S4")]
    
    [(and (equal? (state-s sn) "S1") (equal? machineinput "q")) (make-state "S1")]
    [(and (equal? (state-s sn) "S1") (equal? machineinput "x")) (make-state "S1")]
    [(and (equal? (state-s sn) "S1") (equal? machineinput "u")) (make-state "S2")]
    [(and (equal? (state-s sn) "S1") (equal? machineinput "a")) (make-state "S3")]
    [(and (equal? (state-s sn) "S1") (equal? machineinput "b")) (make-state "S3")]
    [(and (equal? (state-s sn) "S1") (equal? machineinput "d")) (make-state "S4")]
    
    [(and (equal? (state-s sn) "S2") (equal? machineinput "u")) (make-state "S2")]
    [(and (equal? (state-s sn) "S2") (equal? machineinput "a")) (make-state "S3")]
    [(and (equal? (state-s sn) "S2") (equal? machineinput "b")) (make-state "S3")]
    [(and (equal? (state-s sn) "S2") (equal? machineinput "d")) (make-state "S4")]
    
    [(and (equal? (state-s sn) "S3") (equal? machineinput "a")) (make-state "S3")]
    [(and (equal? (state-s sn) "S3") (equal? machineinput "b")) (make-state "S3")]
    [(and (equal? (state-s sn) "S3") (equal? machineinput "d")) (make-state "S4")]
    
    [(and (equal? (state-s sn) "S4") (equal? machineinput "e")) (make-state "S4")]
    [(and (equal? (state-s sn) "S4") (equal? machineinput "f")) (make-state "S4")]
    [else sn]
    ))

;; TESTS:

(begin-for-test
  (check-equal? (next-state (make-state "S0") "q") (make-state "S1") "it should return S1")
  
  (check-equal? (next-state (make-state "S0") "d") (make-state "S4") "it should return S4"))


;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES:
;;
;; STRATEGY: Use template for State on state

(define (accepting-state? sn)
  ( equal? (state-s sn) "S4"
           ))

;; TESTS:

(begin-for-test
  (check-equal? (accepting-state? (make-state "S0") ) #f "it should return S4")
  
  (check-equal? (accepting-state? (make-state "S4") ) #t "it should return S4"))






;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
;; EXAMPLES:
;; (error-state (make-state "S0")) =  #f

;; STRATEGY: Use template for State on state

(define (error-state? sn)
  (cond
    [(equal? (state-s sn) "S0") #f]
    [(equal? (state-s sn) "S1") #f]
    [(equal? (state-s sn) "S2") #f]
    [(equal? (state-s sn) "S3") #f]
    [(equal? (state-s sn) "S4") #f])
  
  )

;; TESTS:
;; it will always return false, since in my finite state machine we can reach from each state to final or accepting state
(begin-for-test
  (check-equal? (error-state? (make-state "S0") ) #f "it should return false")
  
  (check-equal? (error-state? (make-state "S1") ) #f "it should return true"))




