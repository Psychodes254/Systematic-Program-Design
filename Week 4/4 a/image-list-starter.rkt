;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-list-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; =================
;; Data definitions:

;; ListOFImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images

(define LOI1 empty)
(define LOI2 (cons (rectangle 40 30 "solid" "black") empty))
(define LOI3 (cons (rectangle 45 10 "solid" "black")
                   (cons (rectangle 18 12 "solid" "black")
                         (cons (rectangle 36 20 "solid" "black") empty))))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                   ;BASE CASE
        [else (... (first loi)                 ;Image
                   (fn-for-loi (rest loi)))])) ;NATURAL RECURSION

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Image ListOfImage)
;;  - self-reference: (rest loi) is ListOfImages


;; =================
;; Functions:

;; ListOfImage -> Number
;; produces the sum of the area of all the images in ListOfImage
(check-expect (area? LOI1)    0)
(check-expect (area? LOI2) 1200)
(check-expect (area? LOI3) 1386)

;(define (area? loi) 0) ;stub

(define (area? loi)
  (cond [(empty? loi) 0]                   
        [else (+ (* (image-width (first loi))
                    (image-height (first loi)))                 
                   (area? (rest loi)))]))
