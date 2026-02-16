;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname largest-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:

;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 18 empty))
(define LON3 (cons 60 (cons 42 empty)))
(define LON4 (cons 60 (cons 42 (cons 50 (cons 201 (cons 42 empty))))))
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
;; produces tha largest number in the ListOfNumber
(check-expect (largest? LON1)   0)
(check-expect (largest? LON2)  18)
(check-expect (largest? LON3)  60)
(check-expect (largest? LON4) 201)

;(define (largest? lon) 0) ;stub

(define (largest? lon)
  (cond [(empty? lon) 0]
        [else
         (if (> (first lon) (largest? (rest lon)))
             (first lon)
              (largest? (rest lon)))]))