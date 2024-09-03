#lang sicp
(define (make-rat n d)
    (define (make-rat-impl x y)
        (let ((g (gcd x y)))
            (cons (/ x g) (/ y g))))

    (if (< d 0) (make-rat-impl (* -1 n) (abs d)) (make-rat-impl n d)))