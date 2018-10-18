#lang racket

; търсене на квадратен корен по метода на Нютон; с итеративно подобряване на приближението
(define (close-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001)
 )

(define (average x y)
  (/ (+ x y) 2)
 )

(define (improve-guess x guess)
  (average guess (/ x guess))
 )

(define (my-sqrt x)
  (define (sqrt-iter x guess)
    (if (close-enough? guess x)
        guess
        (sqrt-iter x (improve-guess x guess))
     )
   )
  (sqrt-iter x 1.0)
)



(define (divides? a b)
  (= 0 (remainder a b))
 )
(define (isPrime? n)
  (define root (sqrt n))
  (define (helper i)
    (if (> i root)
        #t
        (if (divides? n i)
            #f
            (helper (+ 1 i))
         )
     )
   )
  (if (= 1 n)
      #f
      (helper 2)
   )
 )



; проверка дали цифрите на число са в (нестрого) нарастващ ред
; (increasing? 111) === #t
; (increasing? 151) === #f
; (increasing? 111) === #t
(define (increasing? x)
  (define (help a smallest)
    (let ((last-a (remainder a 10))
          (first-a (quotient a 10))
          )
      (if (= a 0)
          #t
          (and (<= last-a smallest) (help first-a last-a))
       )
     )
   )
  (help x 9)
 )