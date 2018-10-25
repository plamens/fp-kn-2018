#lang racket

; конструиране на дроб
(define (make-rat a b)
  (define g (gcd a b))
  (cons (/ a g) (/ b g))
 )

; достъпване на числителя
(define (num r)
  (car r)
 )

; достъпване на знаменателя
(define (denom r)
  (cdr r)
 )

(define (*rat r1 r2)
  (make-rat
       (* (num r1) (num r2))
       (* (denom r1) (denom r2))
    )
  )

(define half1 (make-rat 1 2))
(define half2 (make-rat 2 4))

(define (print-rat r)
  (printf "~a/~a\n" (num r) (denom r))
 )

(print "1/2 * 2/4 :")
(print-rat (*rat half1 half2))

; съкращаване на съкратима дроб
(define (reduce r)
  (define n (num r))
  (define d (denom r))
  (define g (gcd n d))
  (make-rat (/ n g) (/ d g))
 )

(print "reduce 28/32 :")
(print-rat (reduce (make-rat 28 32)))


(define (+rat r1 r2)
  (make-rat
      (+ (* (num r1) (denom r2)) (* (num r2) (denom r1)))
      (* (denom r1) (denom r2))
   )
 )

(print "1/2 + 1/5 :")
(print-rat (+rat (make-rat 1 2) (make-rat 1 5)))

(define (=rat r1 r2)
  (and
   (= (num r1) (num r2))
   (= (denom r1) (denom r2))
  )
 )

(print "1/2 == 2/4")
(=rat (make-rat 1 2) (make-rat 2 4))
(print "1/2 == 2/3")
(=rat (make-rat 1 2) (make-rat 2 3))

; Списък е:
; - празният спипък '()
; - двойка с втори елемент - спипък
'()
(cons 1 '())
(cons 1 (cons 2 (cons 3 '())))
(list 1 2 3)

(define xs (list 1 2 3))

; проверка дали нещо е спипък
(list? xs)

; дължина на списък
(length xs)

; проверка дали нещо е празния списък
(print "null? (list 1 2 3)")
(null? xs)
(print "null? '()")
(null? '())

; проверка дали нещо се съдържа в списък
(print "member 5 (list 1 2 3)")
(member 5 xs) ; #f ако го няма
(print "member 2 (list 1 2 3)")
(member 2 xs) ; подсписък от елемента до края, ако го има

; cons == prepend
(cons 4 (list 1 2 3))

(print "(car (list 1 2 3)")
(car xs) ; първи елемент на списък
(print "(cdr (list 1 2 3)")
(cdr xs) ; опашка на списък

; долепяне на 2 списъка
(append (list 1 2 3) (list 4 5 6))

; обръщане на списък
(reverse (list 1 2 3))

; head == car
(define (head xs)
  (car xs)
 )
; tail == cdr
(define (tail xs)
  (cdr xs)
 )

(define (my-length xs)
   (if (null? xs)
       0
       (+ 1 (my-length (cdr xs)))
    )
 )

(print "(my-length (list 1 2 3)")
(my-length (list 1 2 3))


(define (my-member x xs)
  (if (null? xs)
      #f
      (if (= (car xs) x)
          #t
          (my-member x (cdr xs))
       )
   )
 )

(print "my-member 2 (list 1 2 3)")
(my-member 2 (list 1 2 3))
(print "my-member 5 (list 1 2 3)")
(my-member 5 (list 1 2 3))


(define (my-reverse xs)
  (define (my-reverse-helper remeaning reversed)
    (if (null? remeaning)
        reversed
        (my-reverse-helper (cdr remeaning) (cons (car remeaning) reversed))
      )
   )
  (my-reverse-helper xs '())
)

(print "my-reverse (list 1 2 3)")
(my-reverse (list 1 2 3))