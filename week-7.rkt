#lang racket

; Зад. 1 Дефинирайте функцията (reverse-column i xss),
; която приема матрица xss (представена като списъс от списъци)
; и индекс на колона i (започващ от нула) и обръща елементите
; и връща матрица, в която елементите на i-тата колона са
; обърнати.
; Примери:
;
; (reverse-column 0 '((1 2 3)    → '((7 2 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (1 8 9))
;
; (reverse-column 1 '((1 2 3)    → '((1 8 3)
;                     (4 5 6)        (4 5 6)
;                     (7 8 9)))      (7 2 9))
(define (get-element i xs)
  (if (= i 0)
      (car xs)
      (get-element (- i 1) (cdr xs))
   )
)

(define (set-element i x xs)
  (if (= i 0)
      (cons x (cdr xs))
      (cons (car xs)
            (set-element (- i 1) x (cdr xs)))
    )
 )

(define (reverse-column i xss)
  (define column
    (map (lambda (xs) (get-element i xs))
         xss))
  (map
    (lambda (x xs) (set-element i x xs))
    (reverse column)
    xss)
)


(define xss '((1 2 3)
              (4 5 6)
              (7 8 9)
              (7 8 9)))

(reverse-column 1 xss)

; Зад. 2 Дефинирайте функция closest,
; която приема списък от числа xs
; и връща друга функция, която приема число y
; и връща най-близкото до y число от xs.
; Пример:
;  Вход: '(3 1 11 7 42)
;  Изход: функцията g, за която 
;        (g 0) -> 1
;        (g 2) -> 1 или 3, по ваш избор
;        (g 14) -> 11
;        (g 6) -> 7
;        (g 33) -> 42
(define (closest xs)
  (lambda (y)
    (let* (
          (distances
           (map
            (lambda (x) (cons x (abs (- y x))))
              xs))
          (closest-pair (foldl
                         (lambda (distance closest-so-far)
                           (if (<
                                (cdr distance)
                                (cdr closest-so-far))
                               distance
                               closest-so-far)
                           )
                         (car distances)
                         distances))
          )
      (car closest-pair))
   )
 )

(define g (closest '(3 1 11 7 42)))
(g 0)
(g 2)
(g 14)
(g 6)
(g 33)
(g -5)
'((3 2) (1 0) (11 10))


;(foldl + 0 '(1 2 3 4))
;(+ 1 0) ;== 1
;(+ 2 1) ;== 3
;(+ 3 3) ;== 6
;(+ 4 6)