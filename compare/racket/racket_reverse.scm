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
     [head car]
     [tail cdr]
     [racket-reverse (lambda (l)
                       (letrec ((aux (lambda (list acc)
                                       (if (null? list)
                                           acc
                                           (aux (tail list) (cons (head list) acc))))))
                         (aux l '())))]
     [make-list (lambda (n)
                  (letrec ((aux (lambda (m acc)
                                  (if (= 0 m)
                                      acc
                                      (aux (- m 1 )(cons E acc))))))
                    (aux n '())))]
     [l (make-list 20000000)]
     )
  (time (racket-reverse l)))
