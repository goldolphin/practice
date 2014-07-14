#lang scheme

;; 1.15
(define (duple n x)
  (if (= n 0)
      '()
      (cons x (duple (- n 1) x))))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))

;; 1.16
(define (invert lst)
  (define (invert2 list2)
    (let ([h (car list2)]
	  [t (cadr list2)])
	  (list t h)))
  (if (empty? lst)
      lst
      (cons (invert2 (car lst)) (cdr lst))))

(invert '((a 1) (a 2) (1 b) (2 b)))

;; 1.17
(define (down lst)
  (match lst
    ['() '()]
    [(list a) (list (list a))]
    [(cons a b) (cons (list a) (down b))]))

(down '(1 2 3))
(down '((a) (fine) (idea)))
