;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname hp-family-tree-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; =================
;; Data definitions:
(define-struct potter (husband wife children))
;; Potter is (make-potter String String ListOfChild)
;; interp. potter is a family with husband wife and their children

;; Child is one of:
;; - String: Child with no further family
;; - Potter: Child who has their own family
;; interp. one child in the family tree

;; ListOfChild is one of:
;; - empty
;; - (cons Child ListOfChild)
;; interp. Children of Potter family

(define D1 (make-potter "Bill Weasley" "Fleur Delacour"
                        (list "Victoire Weasley" "Dominique Weasley" "Louis Weasley")))
(define D2 (make-potter "Percy Weasley" "Audrey Weasley"
                        (list "Molly Weasley" "Lucy Weasley")))
(define D3 (make-potter "George Weasley" "Angelina Johnson"
                        (list "Fred Weasley" "Roxanne Weasley")))
(define D4 (make-potter "Ronald Weasley" "Hermione Granger"
                        (list "Rose Weasley" "Hugo Granger-Weasley")))
(define D5 (make-potter "James Potter" "Lily Luna Potter"
                        (list "Sirius Potter" "Severus Potter" "Luna Potter")))
(define ARTHUR (make-potter "Arthur Weasley" "Molly Prewett"
                            (list "Charlie Weasley" "Fred Weasley" D1 D2 D3 D4 D5)))

;; Templates
#;
(define (fn-for-potter)
  (local [(define (fn-for-child c)
            (cond [(string? c) (...)]
                  [(potter? c) (fn-for-pot c)]))

          (define (fn-for-pot p)
            (... (potter-husband p)
                 (potter-wife p)
                 (fn-for-loc (potter-children p))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-child (first loc))
                        (fn-for-loc (rest loc)))]))]
    (fn-for-pot p)))


;; ListOfChild -> ListOfString
;; return a two element list of patners if husband and wife else ListOfChild
(check-expect (harry-potter D1) (list (list "Bill Weasley" "Fleur Delacour")
                                   "Victoire Weasley" "Dominique Weasley" "Louis Weasley"))
(check-expect (harry-potter D2) (list (list "Percy Weasley" "Audrey Weasley")
                                   "Molly Weasley" "Lucy Weasley"))
(check-expect (harry-potter D3) (list (list "George Weasley" "Angelina Johnson")
                                         "Fred Weasley" "Roxanne Weasley"))
(check-expect (harry-potter D4) (list (list "Ronald Weasley" "Hermione Granger")
                                   "Rose Weasley" "Hugo Granger-Weasley"))
(check-expect (harry-potter D5) (list (list "James Potter" "Lily Luna Potter")
                                         "Sirius Potter" "Severus Potter" "Luna Potter"))
(check-expect (harry-potter ARTHUR) (list (list "Arthur Weasley" "Molly Prewett")
                                       "Charlie Weasley" "Fred Weasley"
                                       (list "Bill Weasley" "Fleur Delacour")
                                       "Victoire Weasley" "Dominique Weasley" "Louis Weasley"
                                       (list "Percy Weasley" "Audrey Weasley")
                                       "Molly Weasley" "Lucy Weasley"
                                       (list "George Weasley" "Angelina Johnson")
                                       "Fred Weasley" "Roxanne Weasley"
                                       (list "Ronald Weasley" "Hermione Granger")
                                       "Rose Weasley" "Hugo Granger-Weasley"
                                       (list "James Potter" "Lily Luna Potter")
                                       "Sirius Potter" "Severus Potter" "Luna Potter")) 
 
;(define (harry-potter p) empty) ;stub

(define (harry-potter p)
  (local [(define (potter--child c)
            (cond [(string? c) (cons c empty)]
                  [(potter? c) (potter--p c)]))

          (define (potter--p p)
            (cons (list (potter-husband p)
                        (potter-wife p))
                  (potter--loc (potter-children p))))

          (define (potter--loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (potter--child (first loc))
                           (potter--loc (rest loc)))]))]
    (potter--p p)))


;; ListOfChild -> String
;; produce name of all the family member in a given tree
(check-expect (potter-name D1) (list "Bill Weasley" "Fleur Delacour" "Victoire Weasley" "Dominique Weasley" "Louis Weasley"))
(check-expect (potter-name D2) (list "Percy Weasley" "Audrey Weasley" "Molly Weasley" "Lucy Weasley"))
(check-expect (potter-name D3) (list "George Weasley" "Angelina Johnson" "Fred Weasley" "Roxanne Weasley"))
(check-expect (potter-name D4) (list "Ronald Weasley" "Hermione Granger" "Rose Weasley" "Hugo Granger-Weasley"))
(check-expect (potter-name D5) (list "James Potter" "Lily Luna Potter" "Sirius Potter" "Severus Potter" "Luna Potter"))
(check-expect (potter-name ARTHUR) (list "Arthur Weasley" "Molly Prewett" "Charlie Weasley" "Fred Weasley"
               "Bill Weasley" "Fleur Delacour" "Victoire Weasley" "Dominique Weasley" "Louis Weasley"
               "Percy Weasley" "Audrey Weasley" "Molly Weasley" "Lucy Weasley"
               "George Weasley" "Angelina Johnson" "Fred Weasley" "Roxanne Weasley"
               "Ronald Weasley" "Hermione Granger" "Rose Weasley" "Hugo Granger-Weasley"
               "James Potter" "Lily Luna Potter" "Sirius Potter" "Severus Potter" "Luna Potter"))
 
;(define (potter-name n) "") ;stub

(define (potter-name n)
  (local [(define (child--name c)
            (cond [(string? c) (cons c empty)]
                  [(potter? c) (potter--name c)]))

          (define (potter--name n)
            (append (list (potter-husband n)
                          (potter-wife n))
                    (potter--lon (potter-children n))))

          (define (potter--lon lon)
            (cond [(empty? lon) empty]
                  [else
                   (append (child--name (first lon))
                           (potter--lon (rest lon)))]))]
    (potter--name n)))