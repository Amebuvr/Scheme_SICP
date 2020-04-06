;Chez Scheme中并未包含error的定义
(define (error message . x)
    (begin 
        (display message)
        (cond ((not (null? x))
                (newline)
                (display x)))
        (exit)))

;(exit)