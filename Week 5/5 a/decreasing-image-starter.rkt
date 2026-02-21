;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname decreasing-image-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define TEXT-SIZE 25)
(define TEXT-COLOR "black")
(define SPACE (text " " TEXT-SIZE TEXT-COLOR))

;; Natural -> Image
;; produce decreasing numbers in a Natural in terms of Image
(check-expect (decreasing-image 0) (text "0" TEXT-SIZE TEXT-COLOR))
(check-expect (decreasing-image 3)  (beside (text "3" TEXT-SIZE TEXT-COLOR) SPACE
                                            (text "2" TEXT-SIZE TEXT-COLOR) SPACE
                                            (text "1" TEXT-SIZE TEXT-COLOR) SPACE
                                            (text "0" TEXT-SIZE TEXT-COLOR)))

;(define (decreasing-image n) "") ;stub

(define (decreasing-image n)
  (cond [(zero? n) (text (number->string n) TEXT-SIZE TEXT-COLOR)]
        [else
         (beside (text (number->string n) TEXT-SIZE TEXT-COLOR)
                 SPACE
              (decreasing-image (sub1 n)))]))