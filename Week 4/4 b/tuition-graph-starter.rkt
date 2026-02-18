;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tuition-graph-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; =================
;; Constants:

(define Y-SCALING 1/200)
(define BAR-COLOUR "lightblue")
(define BAR-WIDTH 30)

(define TEXT-SIZE 24)
(define TEXT-COLOUR "black")


;; =================
;; Data definitions:

(define-struct school (name tuition))
;; school is (make-school String Number)
;; interp. (make-school name tuition) is school with
;;         name is the name of the school
;;         tuition is the amount paid for tuition in Number

(define SCHL-1 (make-school "school1" 40500))
(define SCHL-2 (make-school "school2" 54500))
(define SCHL-3 (make-school "school3" 68400))
(define SCHL-4 (make-school "school4" 39900))
#;
(define (fn-for-schl s)
  (... (school-name s)       ;String
       (school-tuition s)))  ;Number

;; Template rules used:
;;  - compound: 2 fields


;; ListOfSchool is one of:
;;  - empty
;;  - (cons String ListOfSchool)
;; interp. a list of schools

(define SCHL1 empty)
(define SCHL2 (cons SCHL-1 empty))
(define SCHL3 (cons SCHL-2 (cons SCHL-3 (cons SCHL-4 empty))))
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]                   ;BASE CASE
        [else (... (fn-for-schl(first los)                 ;String
                   (fn-for-los (rest los))))])) ;NATURAL RECURSION

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons School ListOfSchool)
;;  - self-reference: (rest los) is ListOfSchool


;; =================
;; Functions:
;; ListOfSchool -> Image
;; return the image of the bar chart representing schools
(check-expect (make-bar SCHL1) (square 40 "outline" "black"))
(check-expect (make-bar SCHL2) (beside/align "bottom"
                                             (overlay/align "center" "bottom"
                                                            (rotate 90(text "school1" TEXT-SIZE TEXT-COLOUR))
                                                            (rectangle BAR-WIDTH (* 40500 Y-SCALING) "outline" TEXT-COLOUR)
                                                            (rectangle BAR-WIDTH (* 40500 Y-SCALING) "solid" BAR-COLOUR))
                                (square 40 "outline" "black")))
(check-expect (make-bar SCHL3) (beside/align "bottom"
                                             (overlay/align "center" "bottom"
                                                            (rotate 90(text "school2" TEXT-SIZE TEXT-COLOUR))
                                                            (rectangle BAR-WIDTH (* 54500 Y-SCALING) "outline" TEXT-COLOUR)
                                                            (rectangle BAR-WIDTH (* 54500 Y-SCALING) "solid" BAR-COLOUR))
                                             (overlay/align "center" "bottom"
                                                            (rotate 90(text "school3" TEXT-SIZE TEXT-COLOUR))
                                                            (rectangle BAR-WIDTH (* 68400 Y-SCALING) "outline" TEXT-COLOUR)
                                                            (rectangle BAR-WIDTH (* 68400 Y-SCALING) "solid" BAR-COLOUR))
                                             (overlay/align "center" "bottom"
                                                            (rotate 90(text "school4" TEXT-SIZE TEXT-COLOUR))
                                                            (rectangle BAR-WIDTH (* 39900 Y-SCALING) "outline" TEXT-COLOUR)
                                                            (rectangle BAR-WIDTH (* 39900 Y-SCALING) "solid" BAR-COLOUR))
                                (square 40 "outline" "black")))
 
;(define (make-bar los) (square 40 "outline" "black")) ;stub

(define (make-bar los)
  (cond [(empty? los) (square 40 "outline" "black")]
        [else (beside/align "bottom"
                            (bar-chart (first los))
                            (make-bar (rest los)))]))


;; School -> Image
;; returns the Image of a bar chart
(check-expect (bar-chart (make-school "school" 20700)) (overlay/align "center" "bottom"
                                                                      (rotate 90(text "school" TEXT-SIZE TEXT-COLOUR))
                                                                      (rectangle BAR-WIDTH (* 20700 Y-SCALING) "outline" TEXT-COLOUR)
                                                                      (rectangle BAR-WIDTH (* 20700 Y-SCALING) "solid" BAR-COLOUR)))

;(define (bar-chart b) (square 40 "outline" "black")) ;stub

(define (bar-chart b)
  (overlay/align "center" "bottom"
                                      (rotate 90(text (school-name b) TEXT-SIZE TEXT-COLOUR))
                                      (rectangle BAR-WIDTH (* (school-tuition b) Y-SCALING) "outline" TEXT-COLOUR)
                                      (rectangle BAR-WIDTH (* (school-tuition b) Y-SCALING) "solid" BAR-COLOUR)))
