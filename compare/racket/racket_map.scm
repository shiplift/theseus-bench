(define-syntax time
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr1 expr ...)
       (syntax/loc
           stx
         (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
           (printf "RESULT-cpu: ~a.0\nRESULT-total: ~a.0\nRESULT-gc: ~a.0\n"
                   cpu user gc)
           (apply values v)))])))
(letrec 
    ([E    'E]
     [F    'F]
     [head car]
     [tail cdr]
     [racket-map (lambda (f l)
                   (if (null? l)
                       '()
                       (cons (f (head l)) (racket-map f (tail l)))))]
     [make-list (lambda (n)
                  (letrec ((aux (lambda (m acc)
                                  (if (= 0 m)
                                      acc
                                      (aux (- m 1 )(cons E acc))))))
                    (aux n '())))]
     [swap (lambda (x)
             (if (eq? x E) F E))]
     [l (make-list 5000000)]
     )
  (time (racket-map swap l)))
