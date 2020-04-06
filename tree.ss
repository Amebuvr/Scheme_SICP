;Chez Scheme并不原生支持树结构，所以以下均为自定义的过程

(load "./io.ss")
(load "./arithmetic.ss")
(load "./list.ss")

(define (count-leaves x)
    (cond 
        ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(define (deep-reverse tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) tree)
        (else (reverse (list (deep-reverse (car tree)) (deep-reverse (cadr tree)))))))

(define (fringe tree)
    (cond 
        ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cadr tree))))))

(define (scale-tree tree factor)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (scale-tree sub-tree factor)
                (* sub-tree factor)))
         tree))

(define (tree-map proc tree)
    (cond 
        ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(define (tree-map proc tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map proc sub-tree)
                (proc sub-tree)))
         tree))
;两种写法事实上没什么不同
;如果Chez Scheme能在底层直接将对数据调用函数和对过程调用函数统一起来就好了
;这样的话就没有什么(tree-map proc tree)了，干脆(proc tree)多方便
;然而果真如此的话，可能不得不摒弃四则运算，这或许就得不偿失了

(define (scale-tree tree factor)
    (tree-map (lambda (x) (* x factor)) tree))
;进一步抽象的scale-tree

(define (square-tree tree)
    (tree-map square tree))

;(define (subsets s)
;    (if (null? s)
;        (list '())  ;注意这是一个包含一个空集的集合
;        (let ((rest (subsets (cdr s))))
;            (append rest (map (lambda (x) (cons (car s) x)) rest)))))
;这个过程更应该包含在list.ss里，我已经将它复制过去了
(exit)