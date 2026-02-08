;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname traffic-light-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A simple traffic light system

;; =================
;; Constants:
(define WIDTH 50)
(define HEIGHT (* WIDTH 3))
(define SPACING 5)

(define SPACE (square SPACING "solid" "black"))
(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))

(define CLR
  (overlay  (above (circle 20 "solid" "red")
                   SPACE
                   (circle 20 "outline" "yellow")
                   SPACE
                   (circle 20 "outline" "green"))
                   SPACE
                   BACKGROUND))
(define CLY
  (overlay  (above (circle 20 "outline" "red")
                   SPACE
                   (circle 20 "solid" "yellow")
                   SPACE
                   (circle 20 "outline" "green"))
                   SPACE
                   BACKGROUND))
(define CLG
  (overlay  (above (circle 20 "outline" "red")
                   SPACE
                   (circle 20 "outline" "yellow")
                   SPACE
                   (circle 20 "solid" "green"))
                   SPACE
                   BACKGROUND))


;; =================
;; Data definitions:

;; LightState is one of:
;;  - "red"
;;  - "yellow"
;;  - "green"
;; interp. the color of a traffic light
#;
(define (fn-for-light-state ls)
  (cond [(string=? "red" ls) (...)]
        [(string=? "yellow" ls) (...)]
        [(string=? "green" ls) (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: "red"
;;  - atomic distinct: "yellow"
;;  - atomic distinct: "green"

;; =================
;; Functions:

;; Traffic -> Traffic
;; start the world with (main "red")
;; no tests for main function
(define (main ls)
  (big-bang ls                         ; Traffic
            (on-tick advance-light 1)  ; Traffic -> Traffic
            (to-draw render-light)))   ; Traffic -> Traffic
            

;; Traffic -> Image
;; produce the next color after a second
(check-expect (advance-light "red") "green")
(check-expect (advance-light "green") "yellow")
(check-expect (advance-light "yellow") "red")

;(define (advance-light ls) "red") ;stub

;<used template from traffic>
(define (advance-light ls)
  (cond [(string=? "red" ls) "green"]
        [(string=? "green" ls) "yellow"]
        [(string=? "yellow" ls) "red"]))


;; Traffic -> Image
;; produce an appropriate outline mode when clock is on 
(check-expect (render-light "red") CLR)
(check-expect (render-light "green") CLG)
(check-expect (render-light "yellow") CLY)

;(define (render-light col) CLG) ;stub

(define (render-light ls)
  (cond [(string=? "red" ls) CLR]
        [(string=? "green" ls) CLG]
        [(string=? "yellow" ls) CLY]))