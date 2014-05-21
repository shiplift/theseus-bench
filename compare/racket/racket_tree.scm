;;; The Computer Language Benchmarks Game
;;; http://benchmarksgame.alioth.debian.org/

;;; Derived from the Chicken variant by Sven Hartrumpf
;;; contributed by Matthew Flatt
;;; Adapted for lamb-bench by Tobias Pape

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

(define (node left val right)
  (let ((v (make-vector 3 #f)))
        (vector-set! v 0 left)
        (vector-set! v 1 val)
        (vector-set! v 2 right)
        v))
(define (node-left n) (vector-ref n 0))
(define (node-val n) (vector-ref n 1))
(define (node-right n) (vector-ref n 2))

;; Instead of (define-struct leaf (val)):
(define (leaf val) (node #f val #f))
(define (leaf? l) (not (node-left l)))
(define (leaf-val l) (node-val l))

(define (make item d)
  (if (= d 0)
      (leaf item)
      (let ((d2 (- d 1)))
        (node (make item d2) item (make item d2)))))

(define (check t)
  (if (leaf? t)
      'E
      (begin
        (check (node-left t))
        (check (node-right t)))))

(define min-depth 3)

(letrec
    ([E    'E]
     [racket-tree
      (lambda (num)
        (letrec ((max-depth num)
                 (stretch-depth (+ max-depth 1))
                 (_ (make E stretch-depth))
                 (long-lived-tree (make E max-depth))
                 (depth-loop (lambda (d)
                               (letrec ([iterations (expt 2 (- max-depth d))]
                                        [iter (lambda (i)
                                                (when (<= i iterations)
                                                  (begin
                                                    (make E d)
                                                    (make E d)
                                                    (iter (+ 1 i)))))])
                                 (iter 1)
                                 (when (< d max-depth)
                                   (depth-loop (+ 1 d)))))))
          (depth-loop min-depth)
          (check long-lived-tree)))]
     [treenum (lambda (l)
                (let* ([pairish (pair? l)]
                       [numberish (if pairish (string->number (car l)) pairish)])
                  (if numberish (/ numberish 1000000) 18)))]
     [num (treenum (vector->list (current-command-line-arguments)))]
     )
  (time (racket-tree num)))
