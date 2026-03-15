;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wide-only-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ListOf Image is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. a list of images

(define I1 empty)
(define I2 (list (rectangle 20 40 "solid" "blue")))
(define I3 (list (rectangle 20 40 "solid" "blue")
                 (rectangle 50 30 "solid" "black")))
(define I4 (list (rectangle 20 40 "solid" "blue")
                 (ellipse 60 30 "outline" "gold")
                 (rectangle 40 20 "solid" "blue")))
(define I5 (list (rectangle 50 10 "solid" "blue")
                 (ellipse 60 30 "outline" "black")
                 (rectangle 20 40 "solid" "blue")
                 (ellipse 20 30 "outline" "black")
                 (rectangle 80 70 "solid" "gold")))

(define (wide? i) (>= (image-width i) (image-height i)))

;; (listof Image) -> (listof Image
;; produce a list of images with width greater than height
(check-expect (wide-only I1) empty)
(check-expect (wide-only I2) empty)
(check-expect (wide-only I3) (list (rectangle 50 30 "solid" "black")))
(check-expect (wide-only I4) (list (ellipse 60 30 "outline" "gold")
                              (rectangle 40 20 "solid" "blue")))
(check-expect (wide-only I5) (list (rectangle 50 10 "solid" "blue")
                 (ellipse 60 30 "outline" "black")
                 (rectangle 80 70 "solid" "gold")))

;(define (wide-only loi) empty) ;stub

(define (wide-only loi) (filter wide? loi))