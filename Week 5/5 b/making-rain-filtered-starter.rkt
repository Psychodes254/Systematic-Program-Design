;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname making-rain-filtered-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))

;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))
(define D2 (make-drop 0 0))
(define D3 (make-drop WIDTH HEIGHT))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD1 empty)
(define LOD2 (cons (make-drop 3 6) empty))
(define LOD3 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 10 20 "button-down") (cons (make-drop 10 20) empty))
(check-expect (handle-mouse empty 10 20        "move")  empty)

;(define (handle-mouse lod x y mevt) empty) ; stub

(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "button-down") (cons (make-drop x y) lod)]
        [else lod]))


;; ListOfDrop -> Image
;; produce arbitary ListOfDrop of images on the screen only
(check-expect (next-drops LOD2) (cons (make-drop 3 (+ SPEED 6)) empty))
(check-expect (next-drops LOD3) (cons (make-drop 10 (+ SPEED 20)) (cons (make-drop 3 (+ SPEED 6)) empty)))

;(define (next-drops i) DROP) ;stub

(define (next-drops lod)
  (check-height (droppings lod)))

;; ListOfDrop -> Image
;; produce filtered and ticked list of drops
(check-expect (droppings LOD1) empty)
(check-expect (droppings LOD2) (cons (make-drop 3 (+ SPEED 6)) empty))
(check-expect (droppings LOD3) (cons (make-drop 10 (+ SPEED 20)) (cons (make-drop 3 (+ SPEED 6)) empty)))

;(define (droppings lod) empty) ; stub

(define (droppings lod)
  (cond [(empty? lod) empty]
        [else
         (cons (next-drop (first lod))
              (droppings (rest lod)))]))


;; Drop -> Drop
;; Produce a drop at next position
(check-expect (next-drop D1) (make-drop 10 (+ SPEED 30)))
(check-expect (next-drop D2) (make-drop 0 (+ SPEED 0)))
(check-expect (next-drop D3) (make-drop WIDTH (+ SPEED HEIGHT)))

;(define (next-drop d) 1) ;stub

(define (next-drop d)
  (make-drop (drop-x d) (+ (drop-y d) SPEED)))


;; ListOfDrop -> Image
;; check the ListOfDrop if on the HEIGHT of the MTS
(check-expect (check-height LOD1) empty)
(check-expect (check-height (cons (make-drop 3 (+ HEIGHT 1)) empty)) empty)
(check-expect (check-height (cons (make-drop 10 (+ HEIGHT 1)) (cons (make-drop 3 6) empty)))
              (cons (make-drop 3 6) empty))

;(define (check-height lod) true) ;stub

(define (check-height lod)
  (cond [(empty? lod) empty]
        [else
         (if (on-screen? (first lod))
             (cons (first lod) (check-height (rest lod)))
             (check-height (rest lod)))]))


;; Image -> Boolean
;; if the image is on the HEIGHT of MTS, return true
(check-expect (on-screen? D1) true)
(check-expect (on-screen? (make-drop WIDTH (+ HEIGHT 1))) false)

;(define (on-screen? i) false) ;stub

(define (on-screen? img)
  (<= (drop-y img) HEIGHT))


;; ListOfDrop -> Image
;; Render the drops onto MTS
(check-expect (render-drops LOD1) MTS)
(check-expect (render-drops LOD2) (place-image DROP 3 6 MTS))
(check-expect (render-drops LOD3) (place-image DROP 10 20
                                               (place-image DROP 3 6 MTS)))

;(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render-drop (first lod) (render-drops (rest lod)))]))


;; Drop -> Image
;; produce a drop placed on on proper x, y position on MTS
(check-expect (render-drop D2 MTS) (place-image DROP 0 0 MTS))
(check-expect (render-drop (make-drop 20 10) MTS) (place-image DROP 20 10 MTS))

;(define (render-drop d img) MTS) ;stub

(define (render-drop d img)
  (place-image DROP (drop-x d) (drop-y d) img))