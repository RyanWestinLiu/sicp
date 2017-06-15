#lang scheme
; 2.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (define (rat-gcd n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (cond ((and (> n 0) (> d 0)) (rat-gcd n d))
        ((and (< n 0) (< d 0)) (rat-gcd (abs n) (abs d)))
        ((and (> n 0) (< d 0)) (rat-gcd (- 0 n) (abs d)))
        (else (rat-gcd n d))))

; 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment xp yp)
  (cons xp yp))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (define (midpoint p1 p2)
    (make-point (/ (+ (x-point p1) (x-point p2)) 2)
                (/ (+ (y-point p1) (y-point p2)) 2)))
  (midpoint (start-segment s) (end-segment s)))

;(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4))))

; 2.3
(define (oblong width height)
  (cons width height))

(define (oblong-width ob)
  (car ob))

(define (oblong-height ob)
  (cdr ob))

(define (oblong-area ob)
  (* (oblong-width ob) (oblong-height ob)))

(define (oblong-perimeter ob)
  (+ (* (oblong-width ob) 2) (* (oblong-height ob) 2)))