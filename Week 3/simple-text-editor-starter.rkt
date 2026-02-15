;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-text-editor-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A simple editor

;; Constants
;; =========

(define WIDTH 300)
(define HEIGHT 20)
(define MTS (empty-scene WIDTH HEIGHT))

(define CURSOR (rectangle 2 14 "solid" "red"))

(define TEXT-SIZE 14)
(define TEXT-COLOUR "black")

;; Data Definitions
;; ================

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))
(define E1 (make-editor "a" ""))
(define E2 (make-editor "" "b"))

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))


;; =================
;; Functions:

;; Editor -> Editor
;; start the world with (main (make-editor "" ""))
;; 
(define (main e)
  (big-bang e                           ; Editor
            (to-draw         render)    ; Editor -> Image
            (on-key    handle-key)))    ; Editor KeyEvent -> Editor


;; Text -> Image
;; render the text on cursor post besides pre on the left side
(check-expect (render (make-editor "" "")) (overlay/align "left" "middle" CURSOR MTS))
(check-expect (render (make-editor "a" "")) (overlay/align "left" "middle" (beside (text "a" TEXT-SIZE TEXT-COLOUR) CURSOR) MTS))
(check-expect (render (make-editor "" "b")) (overlay/align "left" "middle" (beside CURSOR  (text "b" TEXT-SIZE TEXT-COLOUR)) MTS))
(check-expect (render (make-editor "a" "b")) (overlay/align "left" "middle" (beside (text "a" TEXT-SIZE TEXT-COLOUR) CURSOR (text "b" TEXT-SIZE TEXT-COLOUR)) MTS))

;(define (render e) MTS) ;stub

(define (render e)
  (overlay/align "left" "middle"
                 (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                         CURSOR
                         (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
                 MTS))


;; EditorKeyEvent -> Editor
;; Make a simple Editor 
(check-expect (handle-key (make-editor ""     "")     "a") (make-editor "a"    ""))
(check-expect (handle-key (make-editor "a"   "b")     "b") (make-editor "ab"  "b"))
(check-expect (handle-key (make-editor "ab" "bb")    "\b") (make-editor "a"  "bb"))
(check-expect (handle-key (make-editor "aa" "bb")  "left") (make-editor "a" "abb"))
(check-expect (handle-key (make-editor "aa" "bb") "right") (make-editor "aab" "b"))

;(define (handle-key e ke) e) ;stub

(define (handle-key e ke)
  (cond [(key=? ke  "left") (make-editor (string-butlast (editor-pre e)) (string-append (string-butfirst (editor-pre e)) (editor-post e)))]
        [(key=? ke "right") (make-editor (string-append (editor-pre e) (string-first (editor-post e))) (string-last (editor-post e)))]
        [(key=? ke    "\b") (make-editor (string-butlast (editor-pre e)) (editor-post e))]
        [(= 1(string-length ke)) (make-editor (string-append (editor-pre e) ke) (editor-post e))]))
        

;; String -> String
;; return first character of the string
(check-expect (string-first "") "")
(check-expect (string-first "donut") "d")

;(define (string-first e) e) ;stub

(define (string-first e)
  (if (string=? e "")
      ""
      (string-ith e 0)))


;; String -> String
;; return last character of the string
(check-expect (string-last "") "")
(check-expect (string-last "donut") "t")

;(define (string-last e) e) ;stub

(define (string-last e)
  (if (string=? e "")
      ""
      (string-ith e (sub1 (string-length e)))))


;; String -> String
;; returns all the string excluding first character
(check-expect (string-butfirst "") "")
(check-expect (string-butfirst "donut") "onut")

;(define (string-butfirst e) e) ;stub

(define (string-butfirst e)
  (if (string=? e "")
      ""
      (substring e 1)))


;; String -> String
;; returns all the string excluding last character
(check-expect (string-butlast "") "")
(check-expect (string-butlast "donut") "donu")

;(define (string-butlast e) e) ;stub

(define (string-butlast e)
  (if (string=? e "")
      ""
      (substring e 0(sub1 (string-length e)))))