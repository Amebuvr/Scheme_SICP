(load "./io.ss")

(define (square x)
    (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (abs x)
    (cond 
        ((> x 0) x)
        ((= x 0) x)
        ((< x 0) (- x))))

(define (average x y)
    (/ (+ x y) 2))

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))

(define (sqrt x)

    (define (improve guess x)
        (average guess (/ x guess)))
        
    (define (sqrt-iter guess x)
        (if (close-enough? (square guess) x)
            guess
            (sqrt-iter (improve guess x) x)))
            
    (sqrt-iter 1.0 x))

(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (-n 1)))))
        
(define (expt a n)
    (if (= n 0)
        1
        (* a (expt a (- n 1)))))

(define (reminder n q)
    (if (< n q)
        n
        (reminder (- n q) q)))

(define (even? n)
    (= (reminder n 2) 0))

(define (fast-expt a n)
    (cond 
        ((= n 0) 1)
        ((even? n) (square (fast-expt a (/ n 2))))
        (else (* a (fast-expt a (- n 1))))))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (reminder a b))))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

(define (half-interval-method f a b)

    (define (search neg-point pos-point)
        (let ((midpoint (average neg-point pos-point)))
            (if (close-enough? neg-point pos-point)
                midpoint
                (let ((test-value (f midpoint)))
                    (cond 
                        ((positive? test-value) (search neg-point midpoint))
                        ((negative? test-value) (search midpoint pos-point))
                        (else midpoint))))))
                        
    (let ((a-value (f a))
          (b-value (f b)))
        (cond 
            ((and (negative? a-value) (positive? b-value))
             (search a b))
            ((and (negative? b-value) (positive? a-value))
             (search b a))
            (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
   (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(exit)