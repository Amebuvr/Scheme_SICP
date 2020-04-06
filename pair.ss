(load "./io.ss")

;在Chez Scheme中已经包含了序对的定义，但是与下面的略有不同，不过应当是等价的
(define (cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
            ((= m 1) y)
            (else (error "Argument not 0 or 1 --CONS" m))))
    dispatch)

(define (car z)
    (z 0))

(define (cdr z)
    (z 1))

;(exit)