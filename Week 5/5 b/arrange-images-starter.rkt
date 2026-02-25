;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname arrange-images-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;; Constants

(define BLANK (ellipse 10 20 "solid" "blue"))

;; Testing cases
(define T1 (ellipse 30 40 "solid" "blue"))
(define T2 (ellipse 30 60 "solid" "blue"))
(define T3 (ellipse 50 60 "solid" "blue"))

;; ListOfImage is one of:
;;  - image
;;  - (cons Image ListOfImage)
;; interp. an arbitrary number of images

(define LOI-1 empty)
(define LOI-2 (cons T3 (cons T2 (cons T1 empty))))
(define LOI-3 (cons T1 (cons T2 (cons T3 empty))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                   ;BASE CASE
        [else (... (first loi)                 ;Image
                   (fn-for-loi (rest loi)))])) ;NATURAL RECURSION

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Image ListOfImage)
;;  - self-reference: (rest loi) is ListOfImage


;; Functions:

;; ListOfImage -> Image
;; arranges images laid from left to right in increasing order
(check-expect (arrange-to-right LOI-2)
                                (beside T1
                                        T2
                                        T3
                                        BLANK))
(check-expect (arrange-to-right LOI-3) (beside T1
                                               T2
                                               T3
                                               BLANK))
;
;;(define (left-to-right i) BLANK)  ; this is the stub
;
(define (arrange-to-right loi)
  (layout-images (sort-images loi)))


;; ListOfImage -> Image
;; layout images to the right in order
(check-expect (layout-images LOI-1) BLANK)
(check-expect (layout-images LOI-2) (beside T3
                                            T2
                                            T1
                                            BLANK))
(check-expect (layout-images LOI-3) (beside T1
                                            T2
                                            T3
                                            BLANK))

;(define (layout-images loi) BLANK) ;stub

(define (layout-images loi)
  (cond [(empty? loi) BLANK]
        [else (beside (first loi)
                   (layout-images (rest loi)))]))


;; ListOfImage -> Image
;; arrange images in increasing order of size
(check-expect (sort-images LOI-1) empty)
(check-expect (sort-images LOI-3) (cons T1 (cons T2 (cons T3 empty))))
(check-expect (sort-images LOI-2) (cons T1 (cons T2 (cons T3 empty))))
              
;(define (sort-images loi) loi) ;stub

(define (sort-images loi)
  (cond
    [(empty? loi) empty]
    [else (insert-image (first loi) (sort-images (rest loi)))]))


;; Image ListOfImage -> ListOfImage
;; insert img in the correct place from sorted ListOfImage (increasing order of size)
(check-expect (insert-image BLANK LOI-1) (cons BLANK empty))
(check-expect (insert-image T1 (cons T2 (cons T3 empty))) (cons T1 (cons T2 (cons T3 empty))))
(check-expect (insert-image T2 (cons T1 (cons T3 empty))) (cons T1 (cons T2 (cons T3 empty))))
(check-expect (insert-image T3 (cons T1 (cons T2 empty))) (cons T1 (cons T2 (cons T3 empty))))

;(define (insert-image img loi) loi) ;stub

(define (insert-image img loi)
  (cond [(empty? loi) (cons img empty)]
        [else
         (if (larger? img (first loi))
             (cons (first loi)
                   (insert-image img (rest loi)))
             (cons img loi))]))


;; Image Image -> Boolean
;; produce True if img1 is larger than img2 by their area
(check-expect (larger? T1 T2) false)
(check-expect (larger? T2 T1)  true)
(check-expect (larger? T3 T2)  true)

;(define (larger? img1 img2) false) ;stub

(define (larger? img1 img2)
  (> (* (image-width img1) (image-height img1))
     (* (image-width img2) (image-height img2))))