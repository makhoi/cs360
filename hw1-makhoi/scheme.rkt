#lang racket
(require rackunit)
(require rackunit/text-ui)
;;
;; DO NOT REMOVE THESE TWO LINES
;;
(provide (all-defined-out))

;;
;; Problem 1
;;
;; Number Integer -> Number
;;
;; Compute the falling factorial x to the n falling
;;

(define (falling x n)
  (if (= n 0)
      1
      (* x (falling (- x 1) (- n 1))))
  )
;;
;; Problem 2
;;
;; Integer Integer -> Integer
;;
;; Compute the Stirling number of the second kind, n subset k
;;

(define (S n k)
  (cond
    ((= n k) 1)                      ; Case: n = k
    ((or (= k 0) (< k 0)) 0)         ; Case: k = 0 (or invalid case where k < 0)
    ((= n 0) 0)                      ; Case: n = 0 and k > 0
    (else (+ (* k (S (- n 1) k))     ; Recursive case
             (S (- n 1) (- k 1))))))


;;
;; Problem 3
;;
;; List List -> List
;;
;; Produce a list of pairs where the first element of each pair is take from
;; the first arguments xs and the second element of each pair is taken from the
;; second argument, ys.
;;

(define (zip xs ys)
  (cond
    [(or (null? xs) (null? ys)) '()] ; Base case: if either list is empty, return an empty list
    [else (cons (cons (car xs) (car ys)) ; Pair the first elements
                (zip (cdr xs) (cdr ys)))]) ; Recur on the rest of the lists
  )


;;
;; Problem 4
;;
;; ((Any -> Any) -> Integer) -> (Any -> Any)
;;
;; Compute f composed with itself n times.
;; (pow f 0) is the identity function
;; (pow f 1) is f
;;

(define (pow f n)
  (if (= n 0)
      (lambda (x) x) ; Identity function
      (lambda (x) (f ((pow f (- n 1)) x)))))


;;
;; Problem 5
;;
;; (Any -> Number) -> List -> Any
;;
;; Compute the element x in xs for which (f x) is greatest.
;;

(define (argmax f xs)
  (define (helper best best-value rest)
    (if (null? rest)
        best
        (let* ((current (car rest))
               (current-value (f current)))
          (if (> current-value best-value)
              (helper current current-value (cdr rest))
              (helper best best-value (cdr rest))))))
  (let ((first (car xs)))
    (helper first (f first) (cdr xs))))
