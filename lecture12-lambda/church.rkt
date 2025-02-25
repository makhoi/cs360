#lang racket
(require test-engine/racket-tests)

(define zero (lambda (f) (lambda (z) z)))
(define one  (lambda (f) (lambda (z) (f z))))
(define two  (lambda (f) (lambda (z) (f (f z)))))

(define (church->nat n)
  ((n add1) 0))

(define (nat->church n)
  (lambda (f)
    (lambda (z)
      (if (= n 0)
          z
          (f (((nat->church (- n 1)) f) z))))))

;; Evaluate (test) at the DrRacket prompt to run the tests
(check-expect (church->nat zero) 0)
(check-expect (church->nat one) 1)
(check-expect (church->nat two) 2)

(define succ
  'undefined)

(check-expect (church->nat (succ (nat->church 5))) 6)

(define add
  'undefined)

(check-expect (church->nat ((add (nat->church 5)) (nat->church 10))) 15)

(define mult
  'undefined)

(check-expect (church->nat ((mult (nat->church 5)) (nat->church 10))) 50)

