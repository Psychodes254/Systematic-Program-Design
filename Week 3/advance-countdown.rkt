;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname advance-countdown) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A countdown that decreases until it reaches zero

;; =================
;; Constants:

(define WIDTH 200)
(define HEIGHT WIDTH)
(define TEXT-SIZE 20)
(define TEXT-COLOUR "black")
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define MTS (empty-scene WIDTH HEIGHT))
(define CD 1000)

;; =================
;; Data definitions:

;; Countdown is Natural
;; interp. the number of remaining seconds before zero
(define C1 10)  ; start
(define C2 5)   ; middle
(define C3 0)   ; end
#;
(define (advance-countdown CD)
  (if (= CD 0)
      0
      (- CD 1)))

;; Temaplate rules used:
;;  - atomic non-distinct: Integer[0, 10]


;; =================
;; Functions:

;; Countdown -> Countdown
;; start world with (main CD)
(define (main CD)
  (big-bang CD                              ; Countdown
            (on-tick   advance-countdown)   ; Countdown -> Countdown
            (to-draw   render-countdown)  ; Countdown -> Image
            (on-mouse  handle-mouse)))        ; Countdown Integer Integer MouseEvent -> Countdown

;; Countdown -> Countdown
;; produce the next number by advancing it to 1 number less before it reaches zero 
(check-expect (advance-countdown 0)  0)
(check-expect (advance-countdown 10) 9)

(define (advance-countdown CD)
  (if (= CD 0)0
      (- CD 1)))

;; Countdown -> Image
;; render the number at an approriate place on the MTS 
(check-expect (render-countdown 7) (place-image (text (number->string 7) TEXT-SIZE TEXT-COLOUR)
               CTR-X
               CTR-Y
               MTS))

;(define (render-countdown nm) MTS) ;stub

(define (render-countdown CD)
  (place-image (text (number->string CD) TEXT-SIZE TEXT-COLOUR)
               CTR-X
               CTR-Y
               MTS))

; Countdown Integer Integer MouseEvent -> Countdown
;; produce 0 if me is button-down, otherwise produce cd
(check-expect (handle-mouse 2 0 3 "button-down") 0)
(check-expect (handle-mouse 3 2 0 "button-up") 3)

;(define (handle-mouse cd x y me) CD) ;stub

(define (handle-mouse CD x y me)
  (cond [(mouse=? me "button-down") 0]
        [else CD]))