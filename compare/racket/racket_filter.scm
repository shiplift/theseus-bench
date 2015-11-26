#lang racket/base
(require (for-syntax racket/base))
(require (prefix-in r: racket/base))
(require "cons-emulation.rktl")
(define-syntax time
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr1 expr ...)
       (syntax/loc
           stx
         (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
           (printf "0:RESULT-cpu:ms: ~a.0\n0:RESULT-total:ms: ~a.0\n0:RESULT-gc:ms: ~a.0\n"
                   cpu user gc)
           (apply values v)))])))
(struct element (x) #:transparent)
(letrec
    ([e (element 'e)]
     [f (element 'f)]
     [head car]
     [tail cdr]
     [racket-filter (lambda (p l)
                   (cond [(null? l) '()]
                         [(p (head l)) (cons (head l) (racket-filter p (tail l)))]
                         [else (racket-filter p (tail l))]))]
     [make-list (lambda (n)
                  (letrec ((aux (lambda (m acc)
                                  (if (= 0 m)
                                      acc
                                      (aux (- m 1) (cons (if (odd? m) e f) acc))))))
                    (aux n '())))]
     [flt (lambda (x)
             (equal? x e))]
     [listnum (lambda (l)
                (let*
                    ([pairish (r:pair? l)]
                     [numberish (if pairish (string->number (r:car l)) pairish)])
                  (if numberish numberish 5000000)))]
     [num (listnum (vector->list (current-command-line-arguments)))]
     [l (make-list num)])
  (time (void (racket-filter flt l))))
