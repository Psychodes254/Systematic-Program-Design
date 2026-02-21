;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname odd-from-n-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Natural -> Natural
;; produce odd numbers from Natural[0, n]
(check-expect (odd-from-n 0) empty)
(check-expect (odd-from-n 1) (cons 1 empty))
(check-expect (odd-from-n 3) (cons 3 (cons 1 empty)))

;(define (odd-from-n n) empty) ;stub

(define (odd-from-n n)
  (cond [(zero? n) empty]
        [(odd? n) (cons n (odd-from-n (sub1 n)))]
        [else
         (odd-from-n (sub1 n))]))