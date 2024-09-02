#lang sicp
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rect)
  (car rect))

(define (bottom-right rect)
  (cdr rect))

(define (rect-width rect)
  (let ((x2 (x-point (bottom-right rect)))
        (x1 (x-point (top-left rect))))
    (- x2 x1)))

(define (rect-height rect)
  (let ((y2 (y-point (bottom-right rect)))
        (y1 (y-point (top-left rect))))
    (- y2 y1)))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))

(define (rect-perimeter rect)
  (+
   (* 2 (rect-width rect))
   (* 2 (rect-height rect))))