;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-insert) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "string-insert.rkt")

(provide string-insert)

;; string-insert: String PosNum -> String
;; GIVEN: String and a number and string to insert.
;; RETURNS: String "_" added at the given position
;; EXAMPLES :
;; (string-insert "helloworld" 5) = "hello_world"
;; (string-insert "heythere 3) = "hey_there"
;; DESIGN STRATEGY: Combine simpler functions


(define (string-insert s i)
  
  (string-append (string-append (substring s 0 i) "_") (substring s i))
  )


;; TESTS
(begin-for-test
  (check-equal? (string-insert "helloworld" 5) "hello_world" "should return hello_world")
  (check-equal? (string-insert "heythere" 3) "hey_there" "should return hey_there")
  )  