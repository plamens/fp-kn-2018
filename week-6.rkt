#lang racket

(foldl cons '() '(1 2 3 4))
(cons 1 '())
(cons 2 (cons 1 '()))

(foldl + 0 '(1 2 3))
(+ 1 0)
(+ 2 1)
(+ 3 3)

(foldr cons '() '(1 2 3 4))

(foldr append '() '((1 2) (2 3) (6)))

; Зад. 1 Дефинирайте функция от по-висок ред (favg f g),
; която приема две едноаргоментни числови функции f и g
; и връща едноаргоментнa числова функция, чиято стойност
; в точка x е средното аритметично на f и g.
(define favg
  (lambda (f g)
    (lambda (x)
      (/ (+ (f x) (g x)) 2)
     )
   )
 )

((favg sqr sqrt) 5)

; Зад. 2 Дефинирайте функция от по-висок ред (flist fs),
; която приема списък от едноаргоментни функциии
; (list f1 f2 f3 ..)
; и връща едноаргоментнa числова функция, чиято стойност
; в точка x е (f3 (f2 (f1 x))).
(define flist
  (lambda (fs)
    (lambda (x)
      (foldl (lambda (f x) (f x)) x fs))
     )
   )
(define flist-result
  (flist (list (lambda (x) (+ x 3)) sqr)))
(flist-result 5)

(define (flist2 fs)
  (foldl compose identity fs)
 )

((flist2 (list (lambda (x) (+ x 3)) sqr)) 5)


(define fs (list sqr sqrt))
((car fs) 3)
((cadr fs) 3)

(car '(sqr (/ 1 0)))
(cons '+ '(2 5))


'sqr
'(/ 1 0)

; Зад. 3 Да се дефинира функцията
; (sieve-of-eratosthenes n), която приема
; целочисления аргумент n и връща списък с всички
; прости числа по-малки или равни на n, като за
; целта използвайте решетото на Ератостен.
(define (sieve-of-eratosthenes n)
  (define (sieve-helper remaining)
    (if (null? remaining)
        remaining
        (cons
         (car remaining)
         (sieve-helper
          (filter (lambda (x)
                    (< 0 (remainder x (car remaining)))
                    )
                  (cdr remaining))
          )
         )
      )
   )
  (sieve-helper (range 2 n))
 )

(sieve-of-eratosthenes 50)

(list 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
(list 2 3 5 7 9 11 13 15)
(list 2 3 5 7 11 13)