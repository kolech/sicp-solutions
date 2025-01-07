#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (variable? e)
  (symbol? e))

(define (=variable? e v)
  (and (variable? e) (eq? e v)))

(define (same-variable? e1 e2)
  (and (variable? e1) (variable? e2) (eq? e1 e2)))

(define (=number? e v)
  (and (number? e) (= e v)))

(define (concat op a1 rest)
  (append (list op a1) rest))

(define (empty? seq) (= (length seq) 0))

(define (combine-exp op a rest)
  (cond ((empty? rest) a)
        ((and (eq? op '+) (= a 0)) (if (= (length rest) 1) (car rest) (append (list op) rest)))
        ((and (eq? op '*) (= a 1)) (if (= (length rest) 1) (car rest) (append (list op) rest)))
        ((and (eq? op '*) (= a 0)) 0)
        (else (concat op a rest))))

(define (make-exp op a1 a2 rest)
  (define combiner (lambda (x y) (cond ((eq? op '+) (+ x y))
                                       ((eq? op '*) (* x y))
                                       (else (error "unknown operator -- COMBINER" op)))))
  (define identity (cond ((eq? op '+) 0)
                         ((eq? op '*) 1)
                         (else (error "unknown operator -- COMBINER" op))))

  (let ((operands (append (list a1 a2) rest)))
    (let ((term (accumulate combiner identity (filter number? operands)))
          (exp-list (filter (lambda (x) (not (number? x))) operands)))
      (combine-exp op term exp-list))))

(define (make-sum a1 a2 . z)
  (make-exp '+ a1 a2 z))

(define (make-product a1 a2 . z)
  (make-exp '* a1 a2 z))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend s)
  (cadr s))

(define (exptract-rest op e)
  (if (empty? (cdddr e))
      (caddr e)
      (concat op (caddr e) (cdddr e))))

(define (augend e)
  (exptract-rest '+ e))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e)
  (cadr e))

(define (multiplicand e)
  (exptract-rest 'e e))

(define (combine-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (base exponentiation)
  (cadr exponentiation))

(define (exponent exponentiation)
  (caddr exponentiation))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum (make-product (multiplier exp)
                                                (deriv (multiplicand exp) var))
                                  (make-product  (deriv (multiplier exp) var)
                                                 (multiplicand exp))))
        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-product (combine-exponentiation u (- n 1))
                                       (deriv u var)))))
        (else (error "unknown expression type: -- DERIV" exp))))