#lang racket
 
(define (square x)
  (* x x)
)
 
(define (sum-square a b)
  (square (+ a b)))
 
(define (exponent x y)
  (if (= y 0)
      1
      (* x (exponent x (- y 1)))
   )
 )

; (exponent 2 3)
; (* 2 (exponent 2 2))
; (* 2 (* 2 (exponent 2 (- 2 1)))
; (* 2 (* 2 (* 2 (exponent 2 (- 1 1)))
; (* 2 (* 2 (* 2 1)))
; (* 2 (* 2 2))
; (* 2 4)
 
(define (exponent-iter x y)
  (define (exponent-iter-helper x y acc)
    (if (= y 0)
        acc
        (exponent-iter-helper x (- y 1) (* acc x))
     )
   )
  (exponent-iter-helper x y 1)
 )
 
; (exponent-iter 2 3)
; (exponent-iter-helper 2 3 1)
; (exponent-iter-helper 2 2 2)
; (exponent-iter-helper 2 (- 2 1) (* 2 2))
; (exponent-iter-helper 2 1 4)
; (exponent-iter-helper 2 (- 1 1) (* 4 2))
; (exponent-iter-helper 2 0 8)
; 8
 
(define (fib n)
  (define (fib-iter a b x)
    (if (= x n)
        a
        (fib-iter b (+ a b) (+ x 1))
     )
   )
  (fib-iter 0 1 0)
 )
 
(define (sum a b f)
  (define (sum-iter a b acc)
    (if (> a b)
        acc
        (sum-iter (+ a 1) b (+ acc (f a)))
     )
   )
  (sum-iter a b 0)
 )
 
(define (sum-number a b)
  (sum a b identity)
 )
 
(define (sum-squares a b)
  (sum a b square)
 )
 
 
(define (accumulate a b f combine start)
  (define (accumulate-iter a b acc)
    (if (> a b)
        acc
        (accumulate-iter (+ a 1) b (combine acc (f a)))
     )
   )
  (accumulate-iter a b start)
 )

(define (product a b)
  (accumulate a b identity * 1)
 )

(define (apply-func f x n)
  (if (= n 0)
      x
      (apply-func f (f x) (- n 1))
   )
 )
; (apply-func square 2 3) === (square (square (square 2)))