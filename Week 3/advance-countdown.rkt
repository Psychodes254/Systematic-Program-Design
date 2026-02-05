;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname advance-countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A countdown that decreases until it reaches zero

;; =================
;; Constants:

(define WIDTH 600)
(define HEIGHT WIDTH)
(define TEXT-SIZE 20)
(define TEXT-COLOUR "black")
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

;; Countdown is Natural
;; interp. the number of remaining seconds before zero
(define C1 10)  ; start
(define C2 5)   ; middle
(define C3 0)   ; end
#;
(define (advance-countdown cd)
  (if (= cd 0)
      0
      (- cd 1)))

;; Temaplate rules used:
;;  - atomic non-distinct: Integer[0, 10]


;; =================
;; Functions:

;; Countdown -> Countdown
;; advances the countdown by subtracting 1, if the countdown is zero it remains at zero
;; !!!
;(define (advance-countdown cd) 0) ;stub

(define (main cd)
  (big-bang cd                   ; Countdown
            (on-tick   advance-countdown)  ; Countdown -> Countdown
            (to-draw   render-countdown)))   ; Countdown -> Image

;; Countdown -> Countdown
;; produce the next number by advancing it to 1 number less before it reaches zero 
;; !!!
(define (advance-countdown cd)
  (if (= cd 0)
      0
      (- cd 1)))


;; Countdown -> Image
;; render the number at an approriate place on the MTS 
;; !!!
(define (render-countdown cd)
  (place-image (text (number->string cd) TEXT-SIZE TEXT-COLOUR)
               CTR-X
               CTR-Y
               MTS))