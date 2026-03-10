;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname merge-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data Definitions:

;; ListOfNumber is one of:
;; - empty
;; - (cons Number ListOfNumber)
;; interp. a list of numbers

(define LN0 empty)
(define LN1 (cons 1 empty))
(define LN2 (cons 1 (cons 2 empty)))
(define LN3 (cons 1 (cons 2 (cons 3 empty))))

#;
(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else 
         (... (first lon)
              (fn-for-lon (rest lon)))]))

;; ==========
;; Functions:

;; ListOfNumber ListOfNumber -> ListOfNumber
;; return an ascending list of numbers from the two sorted lists
(check-expect (merge-lon empty empty) empty)
(check-expect (merge-lon empty (list 5 6 7 8)) (list 5 6 7 8))
(check-expect (merge-lon (list 8 9 10) empty) (list 8 9 10))
(check-expect (merge-lon (list 50 60 70) (list 80 90 100)) (list 50 60 70 80 90 100))
(check-expect (merge-lon (list 80 90 100) (list 50 60 70)) (list 50 60 70 80 90 100))
(check-expect (merge-lon LN3 LN2) (list 1 2 3))
(check-expect (merge-lon (list 1 4 7) (list 2 3 8)) (list 1 2 3 4 7 8))

;(define (merge-lon lona lonb) LN0) ;stub

(define (merge-lon lona lonb)
  (cond [(empty? lona) lonb]
        [(empty? lonb) lona]
        [(= (first lona) (first lonb)) lona]
        [else (if (<= (first lona) (first lonb))
              (cons (first lona) (merge-lon (rest lona) lonb))
              (cons (first lonb) (merge-lon lona (rest lonb))))]))