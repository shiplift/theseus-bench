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
    ([e 1]
     [head car]
     [tail cdr]
     [racket-reverse (lambda (l)
                       (letrec ((aux (lambda (list acc)
                                       (if (null? list)
                                           acc
                                           (aux (tail list) (cons (head list) acc))))))
                         (aux l '())))]
     [listnum (lambda (l)
                (let*
                    ([pairish (r:pair? l)]
                     [numberish (if pairish (string->number (r:car l)) pairish)])
                  (if numberish numberish 20000000)))]
     [num (listnum (vector->list (current-command-line-arguments)))]
     [l (make-list num e)])
  (time (void (racket-reverse l))))