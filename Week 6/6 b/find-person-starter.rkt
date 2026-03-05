;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname find-person-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Data definitions:

(define-struct person (name age kids))

;; Person is (make-person String Natural ListOfPerson)
;; interp. A person, with first name, age and their children
(define P1 (make-person "N1" 5 empty))
(define P2 (make-person "N2" 25 (list P1)))
(define P3 (make-person "N3" 15 empty))
(define P4 (make-person "N4" 45 (list P3 P2)))
#;
(define (fn-for-person p)
  (... (person-name p)			;String
       (person-age p)			;Natural  
       (fn-for-lop (person-kids p))))   


;; ListOfPerson is one of:
;;  - empty
;;  - (cons Person ListOfPerson)
;; interp. a list of persons
#;
(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-person (first lop))   
              (fn-for-lop (rest lop)))]))


;; Functions:

;; Element String -> Integer or false
;; search the entire tree with a given name, return age, else false
(check-expect (check--lop empty "N2") false)
(check-expect (check--person P1 "N1") 5)
(check-expect (check--lop (cons P1 (cons P2 (cons P3 empty))) "N3" ) 15) 
(check-expect (check--lop (cons P1 (cons P2 (cons P3 empty))) "N4") false)
(check-expect (check--person P2 "N3") false)
(check-expect (check--person P3 "N3") 15)
(check-expect (check--person P4 "N6") false)
(check-expect (check--person P2 "N4") false)
(check-expect (check--person P4 "N1") 5)

;(define (check--person p s) 0)    ;stubs
;(define (check--lop lop s) false)

(define (check--person p s)
  (if (string=? (person-name p) s)
       (person-age p)  
       (check--lop (person-kids p) s)))

(define (check--lop lop s)
  (cond [(empty? lop) false]
        [else
         (if (not (false? (check--person (first lop) s)))
             (check--person (first lop) s)
              (check--lop (rest lop) s))]))