#lang sicp
(define (make-rat n d)
  (let ((c (if (< d 0) -1 1))
        (g (gcd (abs n) (abs d))))
    (cons (/ (* c n) g) (/ (* c d) g))))