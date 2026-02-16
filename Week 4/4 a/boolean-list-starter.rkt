;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname boolean-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:

;; ListOfBooleans is one of:
;;  - empty
;;  - (cons Boolean ListOfBooleans)
;; interp. returns true or false
(define LOB1 empty)
(define LOB2 (cons true  empty))
(define LOB3 (cons false empty))
(define LOB4 (cons true  (cons true  (cons true  (cons true  empty)))))
(define LOB5 (cons false (cons true  (cons false (cons true  empty)))))
(define LOB6 (cons false (cons false (cons false (cons false empty)))))
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (first lob)     
              (fn-for-lob (rest lob)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Boolean ListOfBooleans)
;;  - self-reference: (rest lob) is ListOfBooleans


;; =================
;; Functions:

;; ListOfBooleans -> Boolean
;; return true if all the booleans in ListOfBooleans are true, else return false
(check-expect (all-true? LOB1)  true)
(check-expect (all-true? LOB2)  true)
(check-expect (all-true? LOB3) false)
(check-expect (all-true? LOB4)  true)
(check-expect (all-true? LOB5) false)
(check-expect (all-true? LOB6) false)

;(define (all-true? lob) true) ;stub

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else
          (and (first lob)
              (all-true? (rest lob)))]))