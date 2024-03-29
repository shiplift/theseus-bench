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



(letrec
    ([e 17]
     [head car]
     [tail cdr]
     [racket-append (lambda (a b)
                      (if (null? a)
                          b
                          (cons (head a) (racket-append (tail a) b))))]
     [listnum (lambda (l)
                (let*
                    ([pairish (r:pair? l)]
                     [numberish (if pairish (string->number (r:car l)) pairish)])
                  (if numberish numberish 10000000)))]
     [num (listnum (vector->list (current-command-line-arguments)))]
     [l (make-list num e)]
     [m (make-list num e)])
  (time (void (racket-append l m))))
