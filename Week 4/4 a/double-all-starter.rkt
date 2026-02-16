;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname double-all-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 18 empty))
(define LON3 (cons 60 (cons 42 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Number ListOfNumber)
;;  - self-reference: (rest lon) is ListOfNumber


;; =================
;; Functions:

;; ListOfNumber -> Number
;; produces number times 2
(check-expect (double-all LON1) empty)
(check-expect (double-all LON2) (cons 36 empty))
(check-expect (double-all LON3) (cons 120 (cons 84 empty)))

;(define (double-all lon)  0)  ;stub

(define (double-all lon)
  (cond [(empty? lon) empty]
        [else
            (cons (* (first lon) 2) 
              (double-all (rest lon)))]))