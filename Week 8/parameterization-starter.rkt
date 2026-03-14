;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname parameterization-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ====================
;; String (listof String) -> Boolean
;; Produce true if los contains s
(check-expect (contains? "McGill" empty) false)
(check-expect (contains? "UBC" (cons "McGill" empty)) false)
(check-expect (contains? "UBC" (cons "UBC" empty)) true)
(check-expect (contains? "UBC" (cons "McGill" (cons "UBC" empty))) true)
(check-expect (contains? "McGill" (cons "UBC" empty)) false)
(check-expect (contains? "McGill" (cons "McGill" empty)) true)
(check-expect (contains? "McGill" (cons "UBC" (cons "McGill" empty))) true)

(define (contains? s los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) s)
             true
             (contains? s (rest los)))]))


;; ====================
;; (X -> Y) (listof X)  -> (listof Y)
;; given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 3 4)) (list 9 16))
(check-expect (map2 sqrt (list 9 16)) (list 3 4))
(check-expect (map2 abs (list -9 16 -6)) (list 9 16 6))
(check-expect (map2 string-length (list "mash" "Kim" "John")) (list 4 3 4))

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))


;; ====================
;; (X -> Boolean) (listof X) -> (listof X)
;; Given a list, produce a list of only the elements that satisfy the predicate p
(check-expect (filter2 positive? empty) empty)
(check-expect (filter2 negative? (list 1 -5 5 -1)) (list -5 -1))
(check-expect (filter2 positive? (list 1 -5 5 -1)) (list 1 5))
(check-expect (filter2 false? (list false true false false true)) (list false false false))

(define (filter2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (if (fn (first lon))
             (cons (first lon)
                   (filter2 fn (rest lon)))
             (filter2 fn (rest lon)))]))