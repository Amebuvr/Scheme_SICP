;以下所有函数在Chez Scheme都已经被实现了

(load "./io.ss")
(load "./arithmetic.ss")
;(load "pair.ss")
;如果在这里包含pair.ss的话会导致自定义的car、cdr覆盖掉原有的定义
;而自定义的car、cdr无法处理表

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define (append list1 list2)
    (cond 
        ((null? list1) list2)
        ((null? list2) list1)   ;本句可以不加，略微降慢速度
        (else (cons (car list1) (append (cdr list1) list2)))))

(define (last-pair list)
    (cond 
        ((null? list) (error "List empty --LAST-PAIR"))
        ((null? (cdr list)) list)
        (else (last-pair (cdr list)))))

(define (reverse list)
    (define (iter remain result)
        (if (null? remain)
            result
            (iter (cdr remain) (cons (car remain) result))))
            ;这里的cons不能改成append，因为某些表的car就是单纯的数，而append的定义使它只能接受序对或表
            ;此外，由两个数构成的序对的car和cdr都是数，而表的cdr总是表
    (iter list '()))

(define (map proc items)
    (if (null? items)
        '()
        (cons (proc (car items)) (map proc (cdr items)))))

(define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))
;这是个自定义过程

(define (square-list items)
    (map square items))
;这是个自定义过程

(define (for-each proc items)
    (cond ((not (null? items)) 
            (proc (car items))
            (for-each proc (cdr items)))))

(define (subsets s)
    (if (null? s)
        (list '())  ;注意这是一个包含一个空集的集合
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;(exit)