;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname prefix-equal-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data Definitions:

;; ListOfString is one of:
;; - empty
;; - (cons String ListOfString)
;; interp. a list of strings

(define LS0 empty)
(define LS1 (cons "a" empty))
(define LS2 (cons "a" (cons "b" empty)))
(define LS3 (cons "c" (cons "b" (cons "a" empty))))

#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else 
         (... (first los)
              (fn-for-los (rest los)))]))

;; ==========
;; Functions:

;; ListOfString ListOfString -> Boolean
;; return true if the second list is a prefix of the first list
(check-expect (prefix? empty                       empty)  true)
(check-expect (prefix? empty                  (list "x"))  true)
(check-expect (prefix? (list "x")                  empty) false)
(check-expect (prefix? (list "x" "y")         (list "x")) false)
(check-expect (prefix? (list "x")         (list "x" "y"))  true)
(check-expect (prefix? (list "x")         (list "x" "y"))  true)
(check-expect (prefix? (list "x" "y" "z") (list "x" "y")) false)
(check-expect (prefix? (list "x")     (list "x" "y" "z"))  true)

;(define (prefix? losa losb) false) ;stub

(define (prefix? losa losb)
  (cond [(empty? losa) true]
        [(empty? losb) false]
        [else (and (string=? (first losa) (first losb))
              (prefix? (rest losa) (rest losb)))]))