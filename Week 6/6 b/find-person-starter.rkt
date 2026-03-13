;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname find-person-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(check-expect (search-person P1 "N1") 5)
(check-expect (search-person P2 "N3") false)
(check-expect (search-person P3 "N3") 15)
(check-expect (search-person P4 "N6") false)
(check-expect (search-person P2 "N4") false)
(check-expect (search-person P4 "N1") 5)

;(define (search-person p s) 0)    ;stubs
;(define (check--lop lop s) false)

(define (search-person p s)
  (local [(define (check--person p s)
            (if (string=? (person-name p) s)
                (person-age p)  
                (check--lop (person-kids p) s)))

          (define (check--lop lop s)
            (cond [(empty? lop) false]
                  [else
                   (local [(define check (check--person (first lop) s))]
                   (if (not (false? check))
                       check
                       (check--lop (rest lop) s)))]))]
    (check--person p s)))