;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname designing-with-lists-1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:

;; ListOfNumber is one of:
;;  -empty
;;  -(cons Number ListOfNumber)
;; interp. the number in a list is  weigtht of an owl in ounces
(define LON1 empty)
(define LON2 (cons 56 empty))
(define LON3 (cons 63 (cons 48 empty)))
#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Numbers ListOfNumber)
;;  - self reference: (rest lon) ListOfNumber


;; =================
;; Functions:

;; ListOfNumber -> Number
;; sum the sum ounces in the ListOfNumber of owls weight
(check-expect (sum LON1) 0)
(check-expect (sum LON2) 56)
(check-expect (sum LON3) 111)

;(define (sum lon) 0) ;stub

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
              (sum (rest lon)))]))


;; ListOfNumber -> Natural
;; returns how many owls are in the ListOFNumber of owls weight
(check-expect (total LON1) 0)
(check-expect (total LON2) 1)
(check-expect (total LON3) 2)

;(define (total lon) 0) ;stub

(define (total lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1
              (total (rest lon)))]))