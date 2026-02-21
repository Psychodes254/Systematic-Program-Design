;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname descending-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Natural is one of:
;;  - (cons Number empty)
;;  - (cons Number Number)
;; interp. a natural number
(define N0  5)
(define N1 10)
(define N2  7)  
#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (...
          (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic non-distinct: 0
;;  - compound: (cons Natural Natural)
;;  - self-reference: (sub1 n) is Natural


;; Natural > ListOfNatural
;; produce the list in descending order from between two given Natural
(check-expect (list-from-to N0 N0) (cons 5 empty))
(check-expect (list-from-to N1 N2) (cons 10 (cons 9 (cons 8 (cons 7 empty)))))

;(define (list-from-to n a) 0)

(define (list-from-to n a)
  (cond [(zero? (- n a)) (cons a empty)]
        [else
         (cons n
          (list-from-to (sub1 n) a))]))