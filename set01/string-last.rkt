;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname string-last) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "01" "string-last.rkt")

(provide string-last)

;; string-last: String -> 1String
;; GIVEN: A string
;; RETURNS: the last 1String the String
;; EXAMPLES:
;; (string-last "hello") = "o"
;; (string-last "bye") = "e"
;;DESIGN STRATEGY: Combine simpler functions

(define (string-last s)
  (string-ith s (- (string-length s) 1))
)

;;TESTS
(begin-for-test
  (check-equal? (string-last "hello") "o" "it should return o")
  (check-equal? (string-last "bye") "e" "it should return e")
  )
