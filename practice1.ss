(load "./io.ss")

;练习1.10 Ackermann函数
(define (A x y)
    (cond 
        ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(display (A 1 10)) (newline)
(display (A 2 4)) (newline)
(display (A 3 3)) (newline)

;练习1.11
;递归计算过程
(define (f n)
    (if (< n 3)
        n
        (+ 
            (f (- n 1)) 
            (* 2 (f (- n 2))) 
            (* 3 (f (- n 3))))))

;迭代计算过程
(define (f n)
    (define (f-iter f1 f2 f3 counter)
        (if (> counter n)
            f1
            (f-iter (+ f1 (* 2 f2) (* 3 f3)) f1 f2 (+ counter 1))))
    (if (< n 3)
        n
        (f-iter 2 1 0 3)))

(define (pascal row col)
    (cond 
        ((> col row) (error "Index Out Of Bounds Exception --pascal"))
        ((or (= col 0) (= row col)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(display (pascal 1 2))
(exit)