#lang racket

(define xs1 (list 1 2 3))
(define xs2 (list 5 6 7))

(lambda (x y) (+ x y))

(define plus
  (lambda (x y) (+ x y))
 )

(map (lambda (x) (* x x)) xs1)
(map + xs1 xs2)
(map cons xs1 xs2)

(filter even? xs1)

(define getOp
  (lambda (x)
    (if x
        (lambda (x) (* x x))
        (lambda (x) x)
     )
   )
 )

((getOp #t) 5)

(apply * (range 1 5))
(apply max (range 1 5))

; Зад. 1 Дефинирайте следните функции:
; a). (my-identity x), функцията идентитет: връща каквото и дадете.
; б). (my-compose f g), която връща композицията на функциите f и g.
; в). (my-negate p?), която приема предикат p? и връща предиката (not p?).
; г). (my-curry f x), която приема двуаргумента функция f и първи аргумент x
;      и връща функцията получена от частичното прилагане на x върху f.
; д). (repeatf f n), която връща n-кратната композиция на функцията f.

(define my-identity
  (lambda (x) x)
 )

(define plus1
  (curry + 1)
 )

(define my-compose
  (lambda (f g)
    (lambda (x)
      (f (g x))
     )
   )
 )
(define sq-2
  (my-compose
   (lambda (x) (* x x))
   (lambda (x) (* x x))
  )
 )
(sq-2 2)

(define my-negate
  (lambda (p?)
    (lambda (x)
      (not (p? x))
     )
   )
 )
(define my-negate2
  (lambda (p?)
    (my-compose not p?)
   )
 )
((my-negate even?) 5)
((my-negate2 even?) 5)

(define my-curry
  (lambda (f x)
    (lambda (y)
      (f x y)
     )
   )
 )
((my-curry * 5) 6)

(define repeatf
  (lambda (f n)
   (if (= n 0)
    identity
    (compose f (repeatf f (- n 1)))
    )
   )
 )

((repeatf (lambda (x) (* x x)) 3) 2)

; Зад. 2 Да се дефинира функцията (sum-of-odd-squares xs),
; която връща сбора на квадратите на нечетните числа
; в списъка xs.
; (sum-of-odd-squares (list 3 5 8)) == 34
; (sum-of-odd-squares (list 2 4 6)) == 0

(define (sum-of-odd-squares xs)
  (let* (
    (odd (filter odd? xs))
    (squares (map (lambda (x) (* x x)) odd))
    )
    (apply + squares)
   )
 )
(sum-of-odd-squares (list 3 5 8))
(sum-of-odd-squares (list 2 4 6))