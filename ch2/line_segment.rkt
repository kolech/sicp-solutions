#lang sicp

(define (make-segment x1 y1 x2 y2)
  (cons (make-point x1 y1) (make-point x2 y2)))

(define (start-segment line-segment)
  (car line-segment))

(define (end-segment line-segment)
  (cdr line-segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (mid-point line-segment)
  (let ((x (+
            (x-point (start-segment line-segment))
            (x-point (end-segment line-segment))))
        (y (+
            (y-point (start-segment line-segment))
            (y-point (end-segment line-segment)))))
    (make-point (/ x 2) (/ y 2))))