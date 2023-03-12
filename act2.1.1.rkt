#lang racket
 
;; Ejercicio 1
(define fahrenheit-to-celsius
  (lambda (f)
    (* (- f 32) (/ 5 9))))
    
(fahrenheit-to-celsius 212)

;; Ejercicio 2
(define sign
  (lambda (x1)
    (cond
     [(< x1 0) -1]
     [(> x1 0) 1])))

(sign 4)

;; Ejercicio 3
(define roots
  (lambda (a b c)
    ( /
      (+ (- b)
         (sqrt (- (* b b) (* 4 a c))))
      (* 2 a))))

(roots 2 4 2)

;; Ejercicio 5
(define (factorial x)
  (if (= x 0)
	  1
	  (* x (factorial (- x 1)))))

(factorial 2)

;; Ejercicio 6
(define duplicate
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [else (cons (car lst)(cons (car lst)(duplicate (cdr lst))))])))
  
(duplicate '(1 2 3 4 5))
(duplicate '(a b c d e))

;; Ejercicio 7
(define (pow x y)
  (cond
    [(= y 0) 0]
    [(= y 1) x]
    
    [else
     (* x (pow x (- y 1)))]
))

(pow 3 0)
(pow -2 3)
(pow 11 13)

;; Ejercicio 8
(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 7)
(fib 32)
(map fib (range 10))

;; Ejercicio 9
(define enlist
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [(= (length lst) 1) (cons (cons (car lst) '()) '())]
      [else (cons (cons (car lst) '()) (enlist (cdr lst)))])))

(enlist '())
(enlist '(a b c))
(enlist '((1 2 3) 4 (5) 7 8))

;; Ejercicio 10
(define (positives lst)
  (cond
    [(empty? lst)'()]
    [(> (car lst) 0) (cons (car lst)
                           (positives (cdr lst)))]
    [else (positives (cdr lst))]))

(positives '(-2 -1 2 -3))

;; Ejercicio 11
(define add-list
  (lambda (lst sum)
    (cond
      [(empty? lst) sum]
      [(add-list (cdr lst) (+ sum (car lst)))])))
(add-list '(1 2 3 4 5 6 7) 0)

;; Ejercicio 12
(define invert-pairs
  (lambda (lst)
  (cond
    [(null? lst)'()]
    [(cons (list (cadr (car lst)) (car (car lst)))
            (invert-pairs (cdr lst)))])))
(invert-pairs '((5 6) (7 8) (9 10)))

;; Ejercicio 13
(define list-of-symbols
  (lambda (lst)
    (cond
      [(empty? lst) #t]
      [(not (symbol? (car lst))) #f]
      [else (list-of-symbols (cdr lst))])))
(list-of-symbols '(3 4 5 6 7))
(list-of-symbols '(a b c d e))


;; Ejercicio 15
(define dot-product
  (lambda (lst lst2)
    (cond
      [(empty? (or (and lst lst2) (lst) (lst2))) 0]
      [else (+ (* (car lst) (car lst2)) (dot-product (cdr lst) (cdr lst2)))])))
(dot-product '(1 2 3) '(4 5 6))