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
    [(cons a b) (cons (list a) (down b))]))

(down '(1 2 3))
(down '((a) (fine) (idea)))

;; 1.18
(define (swapper s1 s2 slist)
  (match slist
    ['() '()]
    [(cons h t) (cons
		 (cond
		  [(list? h) (swapper s1 s2 h)]
		  [(equal? h s1) s2]
		  [(equal? h s2) s1]
		  [else h])
		 (swapper s1 s2 t))]))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))

;; 1.19
(define (list-set lst n x)
  (if (= n 0)
      (cons x (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- n 1) x))))

(list-set '(a b c d) 2 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)

;; 1.20
(define (count-occurrences s slist)
  (match slist
    ['() 0]
    [(cons h t) (+
		 (cond
		  [(list? h) (count-occurrences s h)]
		  [(equal? h s) 1]
		  [else 0])
		 (count-occurrences s t))]))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))

;; 1.21
(define (product sos1 sos2)
  (define (product0 s sos)
    (match sos
      ['() '()]
      [(cons h t) (cons (list s h) (product0 s t))]))
  (match sos1
    ['() '()]
    [(cons h t) (append (product0 h sos2) (product t sos2))]))

(product '(a b c) '(x y))

;; 1.22

(define (filter-in pred lst)
  (match lst
    ['() '()]
    [(cons h t) (if (pred h)
		    (cons h (filter-in pred t))
		    (filter-in pred t))]))

(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))

;; 1.23
(define (list-index pred lst)
  (match lst
    ['() #f]
    [(cons h t) (if (pred h)
		    0
		    (let ([n (list-index pred t)])
		      (if n
			  (+ 1 n)
			  #f)))]))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

;; 1.24
(define (every? pred lst)
  (match lst
    ['() #t]
    [(cons h t) (and (pred h)
		    (every? pred t))]))

(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))

;;1.25
(define (exists? pred lst)
  (match lst
    ['() #f]
    [(cons h t) (or (pred h)
		    (exists? pred t))]))

(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))

;;1.26
(define (up lst)
  (match lst
    ['() '()]
    [(cons h t) (cond
		  [(list? h) (append h (up t))]
		  [else (cons h (up t))])]))

(up '((1 2) (3 4)))
(up '((x (y)) z))

