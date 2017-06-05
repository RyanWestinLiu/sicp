#lang scheme

; 1.29
(define (round-to-next-even x)
  (+ x (remainder x 2)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k n))
        y
        (if (even? k)
            (* 2 y)
            (* 4 y))))
  (* (/ h 3) (sum simpson-term 0 inc (round-to-next-even n))))

;(simpson cube 0 1 1000)

; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial-term n)
  (if (odd? n)
      (/ (+ n 1) (+ n 2))
      (/ (+ n 2) (+ n 1))))

(define (factorial n)
  (* 4.0 (product factorial-term 1 inc n)))

;(factorial 100000)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; 1.33
(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                (accumulate combiner null-value term (next a) next b)))
        (else null-value)))

; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (try guess)
    ;(displayln guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;(fixed-point (lambda (x)  (/ (log 1000) (log x))) 2.0)
;(fixed-point (lambda (x) (+ (/ x 2) (/ (log 1000) (* 2 (log x))))) 2.0)

; 1.37
(define (cont-frac n d k)
  (define (f-iter n d i sum)
    (if (= i 1)
        (* (n i) (/ 1 (+ (d i) sum)))
        (f-iter n d (- i 1) (* (n i) (/ 1 (+ (d i) sum))))))
  (define (f n d i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i)
              (f n d (+ i 1))))))
  (f n d 1))

;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           10)

; 1.38
;(cont-frac (lambda (i) 1.0)
;           (lambda (i)
;             (cond ((= i 1) 1.0)
;                   ((= i 2) 2.0)
;                   (else (let ((i (- i 2)))
;                           (let ((remain (remainder i 3))
;                                 (sub (/ i 3)))
;                             (if (= remain 0)
;                                 (* sub 2.0)
;                                 1.0))))))
;           10)

; 1.39
(define (tan-cf x k)
  (define (f x i)
    (displayln i)
    (let ((n (- (* i 2) 1)))
      (cond ((= i 1) (/ x (- 1 (f x (+ i 1)))))
            ((= i k) (/ (* x x) n))
            (else (/ (* x x) (- n (f x (+ i 1))))))))
  (f x 1))

;(tan-cf 1 3)

; 1.40
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.000001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))

;(newton-method (cubic 1 1 1) 1)

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;(((double (double double)) inc) 5)

; 1.42
(define (square x) (* x x))

(define (compose f g)
  (lambda (x) (f (g x))))

;((compose square inc) 6)

; 1.43
(define (repeated f times)
  (define (step f i)
    (if (= i times)
        (lambda (x) (f x))
        (step (lambda (x) (f (f x))) (+ i 1))))
  (step f 1))

((repeated square 2) 5)