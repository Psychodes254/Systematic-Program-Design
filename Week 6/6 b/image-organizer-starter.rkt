;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname image-organizer-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; image-organizer-starter.rkt

;; =================
;; Constants:

(define BLANK (square 10 "solid" "white"))
(define TEXT-COLOR "black")
(define TEXT-SIZE 24)
(define SPACE (rectangle 2 4 "solid" "white"))
 
;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 10 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

;; Templates
#;
(define (fn-for-dir d)
  (... (dir-name d)       
       (fn-for-lod (dir-sub-dirs d))
       (fn-for-loi (dir-images d))))
#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-dir (first lod)
              (fn-for-lod (rest lod))))]))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (first loi)
              (fn-for-loi (rest loi)))]))


;; =================
;; Functions:

;; Dir         -> Integer
;; ListOfDir   -> Integer
;; ListOfImage -> Integer
;; return the total area of images in the directory and all its sub-directories
(check-expect (area--loi empty) 0)
(check-expect (area--loi (list I1 I2)) 200)
(check-expect (area--loi (list I1 I2 I3)) 382)
(check-expect (area--loi empty) 0)
(check-expect (area--lod empty) 0)
(check-expect (area--dir D4) 200)
(check-expect (area--dir D5) 182)
(check-expect (area--dir D6) 382)

;(define (area--dir d) 0)   ;stubs
;(define (area--lod lod) 0)
;(define (area--loi loi) 0)

(define (area--dir d)
  (+ (area--lod (dir-sub-dirs d))
       (area--loi (dir-images d))))

(define (area--lod lod)
  (cond [(empty? lod) 0]
        [else
         (+ (area--dir (first lod))
              (area--lod (rest lod)))]))

(define (area--loi loi)
  (cond [(empty? loi) 0]
        [else (+ (* (image-width (first loi))
                    (image-height (first loi)))
                 (area--loi (rest loi)))]))


;; Dir         -> Image
;; ListOfDir   -> Image
;; ListOfImage -> Image
;; return a rendeered directory with all its images in it and sub-directories
(check-expect (render--loi empty) BLANK)
(check-expect (render--loi (list I1 I2 I3))
              (above/align "right"
                           I1
                           SPACE
                           I2
                           SPACE
                           I3
                           SPACE
                           BLANK))
(check-expect (render--dir D5)
              (beside (text "D5" TEXT-SIZE TEXT-COLOR)
                      BLANK
                      (above/align "right"
                                   I3
                                   SPACE
                                   BLANK)))
(check-expect (render--dir D6)
              (beside (text "D6" TEXT-SIZE TEXT-COLOR)
                      (above/align "right"
                                   (beside (text "D4" TEXT-SIZE TEXT-COLOR)
                                           BLANK
                                           (above/align "right"
                                                        I1
                                                        SPACE
                                                        I2
                                                        SPACE
                                                        BLANK))
                                   (beside (text "D5" TEXT-SIZE TEXT-COLOR)
                                           BLANK
                                           (above/align "right"
                                                        I3
                                                        SPACE
                                                        BLANK))
                                   BLANK)
                      BLANK))


;(define (render--dir d) BLANK)  ;stubs
;(define (render--lod d) BLANK)
;(define (render--loi d) BLANK)

(define (render--dir d)
  (beside (text (dir-name d) TEXT-SIZE TEXT-COLOR)       
       (render--lod (dir-sub-dirs d))
       (render--loi (dir-images d))))

(define (render--lod lod)
  (cond [(empty? lod) BLANK]
        [else
         (above/align "right"
                      (render--dir (first lod))
                      (render--lod (rest lod)))]))

(define (render--loi loi)
  (cond [(empty? loi) BLANK]
        [else
         (above/align "right" (first loi)
                      SPACE
                      (render--loi (rest loi)))]))