;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname product-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; (listof Number) -> Number
;; produce the product of all the numbers in lon
(check-expect (product empty) 1)
(check-expect (product (list 1 2 3)) 6)
(check-expect (product (list 2.5 1 -4)) -10)

(define (product lon0)
  ;; acc: Number; the product of the elements seen so far
  ;; (product (list 1 2 3) 1) ;outer call
  
  ;; (product (list 1 2 3) 1)
  ;; (product (list   2 3) 1)
  ;; (product (list     3) 2)
  ;; (product (list      ) 6)
  (local [(define (product lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (product (rest lon)
                      (* acc (first lon)))]))]
    (product lon0 1)))