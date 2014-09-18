#lang racket/base
(require (for-syntax racket/base))
(require racket/list)
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
(struct element ())
(letrec
    ([E    (element)]
     [head car]
     [tail cdr]
     [racket-append (lambda (a b)
                      (if (null? a) 
                          b
                          (cons (head a) (racket-append (tail a) b))))]
     [listnum (lambda (l)
                (let*
                    ([pairish (pair? l)]
                     [numberish (if pairish (string->number (car l)) pairish)])
                  (if numberish numberish 10000000)))]
     [num (listnum (vector->list (current-command-line-arguments)))]
     [l (make-list num E)]
     [m (make-list num E)]
     )
  (time (void (racket-append l m))))
