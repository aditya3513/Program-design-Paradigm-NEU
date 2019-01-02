;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-delete) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "string-delete.rkt")

(provide string-delete)

;; string-delete: String PosNum -> String
;; GIVEN: String and a number and location of char to delete.
;; RETURNS: char deleted from a string at a given position
;; EXAMPLES :
;; (string-delete "helloworld" 5) = "hellworld"
;; (string-delete "heythere 3) = "hethere"
;; DESIGN STRATEGY: Combine simpler functions


(define (string-delete s i)
  
  (string-append (string-append (substring s 0 (- i 1)) (substring s i))
  )
  )


;; TESTS
(begin-for-test
  (check-equal? (string-delete "helloworld" 5) "hellworld" "should return hellworld")
  (check-equal? (string-delete "heythere" 3) "hethere" "should return hethere")
  )  