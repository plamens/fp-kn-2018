#lang racket

(define x 5)

(define (square x)
  (* x x))

(define (bigger a b)
  (if (> a b)
      a
      b))

(define (sum-square a b)
  (square (+ a b)))

(define (bigger3 a b c)
  (cond
    ((and (> a b) (> a c)) a)
    ((and (> b c) (> b a)) b)
    (else c)))

(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

; Сумата на целите числа >= a и <= b
(define (sum a b)
  (if (= a b)
      a
      (+ b (sum a (- b 1)))))

; Произведението на целите числа >= a и <= b
(define (product a b)
  (if (= a b)
      a
      (* b (product a (- b 1)))))

(define (factorial n)
  (product 1 n))

(define (binom n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(define (last-digit n)
  (remainder n 10))
(define (first-digits n)
  (quotient n 10))

(define (reverse-digits n)
  (define (reverse-digits-helper n acc)
    (if (= n 0)
        acc
        (reverse-digits-helper
         (first-digits n)
         (+ (last-digit n) (* 10 acc)))))
  (reverse-digits-helper n 0))

; Стойности на променливите в reverse-digits-helper:
; n     acc
; 432   0
; 43    2
; 4     23
; 0     234