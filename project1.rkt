#lang racket



(define (count-list-elements x)
  (cond
    ((empty? x) 0)
    (else (+ 1 (count-list-elements (cdr x))))
    ))
;(count-list-elements '(a b c d e f g))


(define (rotate-list-left x)
  (cond
    ((empty? x) empty)
    ((empty? (cdr x)) x)
    (else (append (cdr x) (list(car x))))
  ))
;(rotate-list-left '(a b c d e))


(define (rotate-list-left-n x n)
  (cond
    ((= n 0) x)
    (else (rotate-list-left-n (rotate-list-left x) (- n 1)))
    ))
;(rotate-list-left-n '(a b c d e) 3)


(define (list-element-n x n)
  (cond
    ((= n 0) (car x))
    (else (list-element-n (cdr x) (- n 1)))
    ))
;(list-element-n  '(a b c d e f) 3)


(define (list-minus-element-n x n)
  (cond
    ((= n 0) (cdr x))
    (else (cons (car x) (list-minus-element-n (cdr x) (- n 1))))
    ))
;(list-minus-element-n '(a b c d e) 2)


(define (rotate-list-right x)
  (let ((n (count-list-elements x)))
    (cons (list-element-n x (- n 1)) (list-minus-element-n x (- n 1) ))
  ))
;(rotate-list-right '(a b c d e f))
  



(define (reverse-list x)
  (cond
    ((empty? x) x)
    ((empty? (cdr x)) x)
    (else (append (reverse-list (cdr x)) (list (car x))))
    ))

;(reverse-list '(a b c d e f))



(define (cons-to-all a x)
  (map (lambda (z) (cons a z)) x)
  )

;(cons-to-all 'w '((b c) (y z) (m n)))




(define z '())
(define (permute x)
  ;(define z '())
  (cond
    ((empty? x) empty)
    ((empty? (cdr x)) (append (list x) z))
    ((empty? (cdr (cdr x))) (append (list x) (list(reverse-list x))))
    (else (ph-2 x))
  ))


(define (ph-2 x)
  (define m (count-list-elements x))
  (define z '())
  (for ((i (in-inclusive-range 0 (- m 1))))
    (set! z (append z (ph-1 x i)))
    )
  z
  )

  
(define (ph-1 x n)
  (cons-to-all (list-element-n x n) (permute (list-minus-element-n x n)))
  )



(permute '(a b c d))

  
  


