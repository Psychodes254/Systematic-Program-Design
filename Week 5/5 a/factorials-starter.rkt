;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname factorials-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Natural is one of:
;;  - 1
;;  - (fact Natural)
;; interp. a natural number
(define N0 0)  ;1
(define N1 1)  ;1
(define N2 2)  ;2
(define N3 3)  ;6
(define N4 4)  ;24
#;
(define (fn-for-natural n)
  (cond [(zero? n) (...)]
        [else
         (...
          (fn-for-natural (sub1 n)))]))

;; Template rules used:
;;  - one-of: two cases
;;  - atomic distinct: 1
;;  - compound: (add1 Natural)
;;  - self-reference: (sub1 n) is Natural


;; Natural -> Natural
;; compute n * n-1 * n-2 * ... * 1
(check-expect (fact N0) 1)
(check-expect (fact N1) 1)
(check-expect (fact N2) 2)
(check-expect (fact N3) 6)
(check-expect (fact N4) 24)

;(define (fact n) 1) ;stub

(define (fact n)
  (cond [(zero? n) 1]
        [else
         (* n
          (fact (sub1 n)))]))